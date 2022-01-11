pub const mco_state = extern enum(c_int) {
    Dead = 0,
    Normal = 1,
    Running = 2,
    Suspended = 3,
    _,
};
pub const mco_result = extern enum(c_int) {
    Success = 0,
    GenericError = 1,
    InvalidPointer = 2,
    InvalidCoroutine = 3,
    NotSuspended = 4,
    NotRunning = 5,
    MakeContextError = 6,
    SwitchcontextError = 7,
    NotEnoughSpace = 8,
    OutOfMemory = 9,
    InvalidArguments = 10,
    InvalidOperation = 11,
    _,
};
pub const mco_coro = extern struct {
    context: ?*c_void,
    state: mco_state,
    func: *fn (*mco_coro) callconv(.C) void,
    prev_co: ?*mco_coro,
    user_data: ?*c_void,
    allocator_data: ?*c_void,
    free_cb: ?fn (?*c_void, ?*c_void) callconv(.C) void,
    stack_base: ?*c_void,
    stack_size: usize,
    storage: ?*u8,
    bytes_stored: usize,
    storage_size: usize,
    asan_prev_stack: ?*c_void,
    tsan_prev_fiber: ?*c_void,
    tsan_fiber: ?*c_void,
};
pub const mco_desc = extern struct {
    func: ?fn (*mco_coro) callconv(.C) void,
    user_data: ?*c_void,
    malloc_cb: ?fn (usize, ?*c_void) callconv(.C) ?*c_void,
    free_cb: ?fn (?*c_void, ?*c_void) callconv(.C) void,
    allocator_data: ?*c_void,
    storage_size: usize,
    coro_size: usize,
    stack_size: usize,
};
pub extern "C" fn mco_desc_init(arg_func: fn (*mco_coro) callconv(.C) void, arg_stack_size: usize) mco_desc;
pub extern "C" fn mco_init(arg_co: *mco_coro, arg_desc: *mco_desc) mco_result;
pub extern "C" fn mco_uninit(arg_co: *mco_coro) mco_result;
pub extern "C" fn mco_create(arg_out_co: **mco_coro, arg_desc: *const mco_desc) mco_result;
pub extern "C" fn mco_destroy(arg_co: *mco_coro) mco_result;
pub extern "C" fn mco_resume(arg_co: *mco_coro) mco_result;
pub extern "C" fn mco_yield(arg_co: *mco_coro) mco_result;
pub extern "C" fn mco_status(arg_co: *mco_coro) mco_state;
pub extern "C" fn mco_get_user_data(arg_co: *mco_coro) ?*c_void;
pub extern "C" fn mco_push(arg_co: *mco_coro, arg_src: ?*const c_void, arg_len: usize) mco_result;
pub extern "C" fn mco_pop(arg_co: *mco_coro, arg_dest: ?*c_void, arg_len: usize) mco_result;
pub extern "C" fn mco_peek(arg_co: *mco_coro, arg_dest: ?*c_void, arg_len: usize) mco_result;
pub extern "C" fn mco_get_bytes_stored(arg_co: *mco_coro) usize;
pub extern "C" fn mco_get_storage_size(arg_co: *mco_coro) usize;
pub extern "C" fn mco_running() *mco_coro;
pub extern "C" fn mco_result_description(arg_res: mco_result) [*:0]const u8;
