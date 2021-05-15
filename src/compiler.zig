const std = @import("std");

usingnamespace @import("common.zig");
usingnamespace @import("job.zig");
usingnamespace @import("location.zig");
usingnamespace @import("lexer.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("error_handler.zig");
usingnamespace @import("types.zig");
usingnamespace @import("symbol.zig");
usingnamespace @import("code_formatter.zig");
usingnamespace @import("code_runner.zig");
usingnamespace @import("dot_printer.zig");
usingnamespace @import("ring_buffer.zig");

pub const SourceFile = struct {
    path: StringBuf,
    content: StringBuf,

    asts: List(*Ast),

    const Self = @This();

    pub fn init(path: StringBuf, content: StringBuf, allocator: *std.mem.Allocator) Self {
        return Self{
            .path = path,
            .content = content,
            .asts = List(*Ast).init(allocator),
        };
    }

    pub fn deinit(self: *const Self) void {
        self.path.deinit();
        self.content.deinit();
        self.asts.deinit();
    }
};

pub const Compiler = struct {
    allocator: *std.mem.Allocator,
    constantsAllocator: std.heap.ArenaAllocator,
    stackAllocator: std.heap.ArenaAllocator,
    astAllocator: std.heap.ArenaAllocator,

    errorReporter: *ErrorReporter,
    errorMsgBuffer: std.ArrayList(u8),

    typeRegistry: TypeRegistry,

    files: List(*SourceFile),

    globalScope: *SymbolTable,

    allFibers: List(*FiberContext),
    fiberPool: List(*FiberContext),
    unqueuedJobs: RingBuffer(*Job),
    fiberQueue1: RingBuffer(*FiberContext),
    fiberQueue2: RingBuffer(*FiberContext),
    readyFibers: *RingBuffer(*FiberContext) = undefined,
    waitingFibers: *RingBuffer(*FiberContext) = undefined,

    const Self = @This();

    pub fn init(allocator: *std.mem.Allocator, errorReporter: *ErrorReporter) !*Self {
        var self = try allocator.create(Self);
        var globalScope = try allocator.create(SymbolTable);

        self.* = Self{
            .allocator = allocator,
            .constantsAllocator = std.heap.ArenaAllocator.init(allocator),
            .stackAllocator = std.heap.ArenaAllocator.init(allocator),
            .astAllocator = std.heap.ArenaAllocator.init(allocator),

            .errorReporter = errorReporter,
            .typeRegistry = try TypeRegistry.init(allocator),

            .files = List(*SourceFile).init(allocator),

            .globalScope = globalScope,

            .allFibers = List(*FiberContext).init(allocator),
            .fiberPool = List(*FiberContext).init(allocator),
            .unqueuedJobs = try RingBuffer(*Job).init(allocator),
            .fiberQueue1 = try RingBuffer(*FiberContext).init(allocator),
            .fiberQueue2 = try RingBuffer(*FiberContext).init(allocator),

            .errorMsgBuffer = std.ArrayList(u8).init(allocator),
        };

        globalScope.* = SymbolTable.init(null, &self.constantsAllocator.allocator, allocator);
        self.readyFibers = &self.fiberQueue1;
        self.waitingFibers = &self.fiberQueue2;

        (try globalScope.define("void")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getVoidType() } } };

        (try globalScope.define("bool")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getBoolType(1) } } };
        (try globalScope.define("b8")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getBoolType(1) } } };
        (try globalScope.define("b16")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getBoolType(2) } } };
        (try globalScope.define("b32")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getBoolType(4) } } };
        (try globalScope.define("b64")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getBoolType(8) } } };

        (try globalScope.define("i8")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getIntType(1, true, null) } } };
        (try globalScope.define("i16")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getIntType(2, true, null) } } };
        (try globalScope.define("i32")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getIntType(4, true, null) } } };
        (try globalScope.define("i64")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getIntType(8, true, null) } } };
        (try globalScope.define("i128")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getIntType(16, true, null) } } };
        (try globalScope.define("u8")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getIntType(1, false, null) } } };
        (try globalScope.define("u16")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getIntType(2, false, null) } } };
        (try globalScope.define("u32")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getIntType(4, false, null) } } };
        (try globalScope.define("u64")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getIntType(8, false, null) } } };
        (try globalScope.define("u128")).* = Symbol{ .kind = .{ .Type = .{ .typ = try self.typeRegistry.getIntType(16, false, null) } } };

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.typeRegistry.deinit();
        self.errorMsgBuffer.deinit();

        // deinit fibers
        for (self.allFibers.items) |fiber| {
            fiber.deinit();
        }
        self.allFibers.deinit();
        self.fiberPool.deinit();
        self.fiberQueue1.deinit();
        self.fiberQueue2.deinit();
        self.unqueuedJobs.deinit();

        for (self.files.items) |file| {
            file.deinit();
            self.allocator.destroy(file);
        }
        self.files.deinit();

        self.globalScope.deinit();
        self.allocator.destroy(self.globalScope);
        self.constantsAllocator.deinit();
        self.stackAllocator.deinit();
        self.astAllocator.deinit();

        self.allocator.destroy(self);
    }

    pub fn reportError(self: *Self, location: *const Location, comptime format: []const u8, args: anytype) void {
        self.errorMsgBuffer.resize(0) catch unreachable;
        std.fmt.format(self.errorMsgBuffer.writer(), format, args) catch {};
        self.errorReporter.report(self.errorMsgBuffer.items, location);
    }

    pub fn allocateSourceFile(self: *Self, path: StringBuf, content: StringBuf) !*SourceFile {
        var file = try self.allocator.create(SourceFile);
        file.* = SourceFile.init(path, content, self.allocator);
        try self.files.append(file);
        return file;
    }

    fn swapReadyAndWaitingQueue(self: *Self) void {
        const temp = self.readyFibers;
        self.readyFibers = self.waitingFibers;
        self.waitingFibers = temp;
    }

    fn getFreeFiber(self: *Self) !*FiberContext {
        if (self.fiberPool.items.len == 0) {
            const fiber = try FiberContext.init(self);
            try self.allFibers.append(fiber);
            return fiber;
        } else {
            return self.fiberPool.pop();
        }
    }

    pub fn allocateAndAddJob(self: *Self, _job: anytype) !*Job {
        var job = try self.allocator.create(@TypeOf(_job));
        job.* = _job;
        try self.addJob(&job.job);
        return &job.job;
    }

    pub fn addJob(self: *Self, job: *Job) !void {
        try self.unqueuedJobs.push(job);
        std.log.debug("[addJob] {}, {}, {}", .{ self.unqueuedJobs.len(), self.readyFibers.len(), self.waitingFibers.len() });
    }

    pub fn run(self: *Self) !void {
        var madeProgress = false;

        while (true) {
            std.log.debug("[Main Loop] {}, {}, {}", .{ self.unqueuedJobs.len(), self.readyFibers.len(), self.waitingFibers.len() });
            var fiber: ?*FiberContext = null;
            if (self.readyFibers.len() == 0 and self.waitingFibers.len() > 0 and madeProgress) {
                std.log.debug("Move jobs from waiting queue to ready queue.", .{});
                self.swapReadyAndWaitingQueue();
                madeProgress = false;
            }
            if (self.readyFibers.pop()) |f| {
                std.log.debug("Take job from ready queue.", .{});
                fiber = f;
            } else if (self.unqueuedJobs.pop()) |job| {
                std.log.debug("Start next job.", .{});
                job.compiler = self;
                fiber = try self.getFreeFiber();
                fiber.?.job = job;
                fiber.?.madeProgress = true;
                fiber.?.wasCancelled = false;
                fiber.?.done = false;
                fiber.?.state = .Suspended;
            } else {
                std.log.debug("No more jobs left.", .{});
                break;
            }

            if (fiber) |f| {
                try f.step();
                madeProgress = madeProgress or f.madeProgress;
                if (f.done) {
                    f.job.?.free(self.allocator);
                    f.job = null;
                    try self.fiberPool.append(f);
                } else {
                    try self.waitingFibers.push(f);
                }
            }
        }

        std.debug.assert(self.unqueuedJobs.len() == 0);
        std.debug.assert(self.readyFibers.len() == 0);

        // Cancel all waiting jobs.
        std.log.debug("There are {} fibers still waiting.", .{self.waitingFibers.len()});
        var it = self.waitingFibers.iterator();
        while (it.next()) |f| {
            std.log.debug("Cancelling job.", .{});
            f.wasCancelled = true;
            try f.step();

            if (f.job) |j| {
                j.free(self.allocator);
            }
        }

        var it2 = self.unqueuedJobs.iterator();
        while (it2.next()) |job| {
            job.free(self.allocator);
        }

        // print ast graph
        for (self.files.items) |file| {
            var newFileName = StringBuf.init(self.allocator);
            try std.fmt.format(newFileName.writer(), "{s}.gv", .{file.path.items});
            defer newFileName.deinit();
            var graphFile = try std.fs.cwd().createFile(newFileName.items, .{});
            defer graphFile.close();
            var dotPrinter = try DotPrinter.init(graphFile.writer(), true);
            defer dotPrinter.deinit(graphFile.writer());

            for (file.asts.items) |ast| {
                try dotPrinter.printGraph(graphFile.writer(), ast);
            }
        }
    }
};
