const std = @import("std");

usingnamespace @import("common.zig");
usingnamespace @import("compiler.zig");
usingnamespace @import("lexer.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("type_checker.zig");
usingnamespace @import("error_handler.zig");
usingnamespace @import("code_formatter.zig");
usingnamespace @import("code_runner.zig");
usingnamespace @import("dot_printer.zig");
usingnamespace @import("job.zig");

pub const Coroutine = @import("stackfull_coroutine.zig").StackfullCoroutine(FiberContext);

pub const FiberWaitCondition = struct {
    const Self = @This();

    evalFn: fn (self: *Self) bool,
    reportErrorFn: fn (self: *Self, compiler: *Compiler) void,

    pub fn eval(self: *Self) bool {
        return self.evalFn(self);
    }

    pub fn reportError(self: *Self, compiler: *Compiler) void {
        self.reportErrorFn(self, compiler);
    }
};

pub const FiberContext = struct {
    madeProgress: bool = false,
    wasCancelled: bool = false,
    done: bool = false,
    state: enum { Running, Suspended } = .Suspended,
    coro: Coroutine,

    compiler: *Compiler,
    job: ?*Job,
    codeRunner: CodeRunner,

    const Self = @This();

    pub fn init(compiler: *Compiler) !*Self {
        var context = try compiler.allocator.create(Self);
        const coro = try Coroutine.init(
            Self.run,
            context,
            compiler.allocator,
        );
        context.* = Self{
            .compiler = compiler,
            .coro = coro,
            .job = null,
            .codeRunner = try CodeRunner.init(compiler),
        };
        return context;
    }

    pub fn deinit(self: *Self) void {
        self.coro.deinit();
        self.codeRunner.deinit();
        self.compiler.allocator.destroy(self);
    }

    pub fn run() void {
        var self = Coroutine.current().getUserData() orelse unreachable;

        while (true) {
            if (self.job) |job| {
                std.log.debug("[{}, {any}] Running next job.", .{ self.madeProgress, self.state });
                self.done = false;
                job.run() catch |err| {
                    std.log.err("Job failed with error: {any}", .{err});
                };
                self.madeProgress = true;
                self.done = true;

                std.log.debug("[{}, {any}] done", .{ self.madeProgress, self.state });
            }
            Coroutine.yield() catch unreachable;
            if (self.wasCancelled) break;
        }
        std.log.debug("[{}, {any}] Cancelled", .{ self.madeProgress, self.state });
    }

    pub fn step(self: *Self) !void {
        _ = try self.coro.call();
    }

    pub fn waitUntil(self: *Self, condition: *FiberWaitCondition) !void {
        var ctx = Coroutine.current().getUserData() orelse unreachable;
        ctx.state = .Suspended;
        defer ctx.state = .Running;
        ctx.madeProgress = true;

        while (true) {
            Coroutine.yield() catch unreachable;
            if (ctx.wasCancelled) {
                condition.reportError(ctx.job.?.compiler.?);
                return error.WaitFailed;
            } else if (condition.eval()) {
                ctx.madeProgress = true;
                return;
            } else {
                ctx.madeProgress = false;
            }
        }
    }
};

pub const Job = struct {
    const Self = @This();

    runFn: fn (self: *Self) anyerror!void,
    freeFn: fn (self: *Self, allocator: *std.mem.Allocator) void,

    compiler: ?*Compiler = null,

    pub fn run(self: *Self) anyerror!void {
        try self.runFn(self);
    }

    pub fn free(self: *Self, allocator: *std.mem.Allocator) void {
        return self.freeFn(self, allocator);
    }
};

pub const CompileAstJob = struct {
    job: Job = Job{
        .runFn = run,
        .freeFn = free,
    },

    ast: *Ast,

    const Self = @This();

    pub fn init(ast: *Ast) Self {
        return Self{
            .ast = ast,
        };
    }

    pub fn free(job: *Job, allocator: *std.mem.Allocator) void {
        const self = @fieldParentPtr(Self, "job", job);
        allocator.destroy(self);
    }

    pub fn run(job: *Job) anyerror!void {
        const self = @fieldParentPtr(Self, "job", job);
        var ctx = Coroutine.current().getUserData() orelse unreachable;
        var compiler = job.compiler orelse unreachable;

        var typeChecker = try TypeChecker.init(compiler, &ctx.codeRunner);
        defer typeChecker.deinit();

        std.log.debug("CompileAstJob: {x}, {x}", .{ @ptrToInt(job), @ptrToInt(self.ast) });
        try typeChecker.compileAst(self.ast, null);
        std.log.debug("After compileAst", .{});

        if (self.ast.typ.is(.Error) or self.ast.typ.is(.Unknown)) {
            const location = &self.ast.location;
            std.log.debug("{s}:{}:{}: Failed to compile top level expr", .{ location.file, location.line, location.column });
        } else {
            try ctx.codeRunner.runAst(self.ast);
        }
    }
};

pub const LoadFileJob = struct {
    job: Job = Job{
        .runFn = run,
        .freeFn = free,
    },

    fileName: String,

    const Self = @This();

    pub fn init(msg: String) Self {
        return Self{
            .msg = msg,
        };
    }

    pub fn free(job: *Job, allocator: *std.mem.Allocator) void {
        const self = @fieldParentPtr(Self, "job", job);
        allocator.destroy(self);
    }

    pub fn run(job: *Job) anyerror!void {
        const self = @fieldParentPtr(Self, "job", job);
        var ctx = Coroutine.current().getUserData() orelse unreachable;
        var compiler: *Compiler = job.compiler orelse unreachable;

        const fileContent = try std.fs.cwd().readFileAlloc(compiler.allocator, self.fileName, std.math.maxInt(usize));

        const fullPath = try std.fs.cwd().realpathAlloc(compiler.allocator, self.fileName);
        var sourceFile = try compiler.allocateSourceFile(
            StringBuf.fromOwnedSlice(compiler.allocator, fullPath),
            StringBuf.fromOwnedSlice(compiler.allocator, fileContent),
        );

        var lexer = try Lexer.init(sourceFile.path.items, sourceFile.content.items);
        var parser = Parser.init(lexer, &compiler.astAllocator.allocator, compiler.errorReporter);
        defer parser.deinit();

        while (try parser.parseTopLevelExpression()) |ast| {
            const j = try compiler.allocateAndAddJob(CompileAstJob.init(ast));
            std.log.debug("create job: {x}, {x}", .{ @ptrToInt(j), @ptrToInt(ast) });
        }

        std.log.debug("[LoadFileJob '{s}'] Done.", .{self.fileName});
    }
};
