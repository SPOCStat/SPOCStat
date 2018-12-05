module CacheInfo
  use, intrinsic :: ISO_C_BINDING, only: c_size_t, c_int
  implicit none

  interface
     function cache_line_size() result(res) bind(c,name="CacheLineSize")
       import :: c_size_t
       integer(c_size_t) :: res
     end function
     function get_page_size() result(res) bind(c,name="GetPageSize")
       import :: c_size_t
       integer(c_size_t) :: res
     end function
  end interface
end module
