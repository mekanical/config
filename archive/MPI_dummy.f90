!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!  single FDTD Program               !!!!
!!!!  FDTD Produced by Takashi Arakawa  !!!!
!!!!  (c) Maeda Tadahiko lab.2008.Oct.  !!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module MPI_dummy
  implicit none
  
  integer, parameter :: MPI_STATUS_SIZE = 1
  integer, parameter :: MPI_COMM_WORLD = 0
  integer, parameter :: MPI_PROC_NULL = -1
  integer, parameter :: MPI_INTEGER = 0
  integer, parameter :: MPI_DOUBLE_PRECISION = 1
  integer, parameter :: MPI_DOUBLE_COMPLEX = 2
  
  integer, parameter :: MPI_SUM = 0
  interface MPI_Isend
     module procedure MPI_Isend_int
     module procedure MPI_Isend_real8
     module procedure MPI_Isend_complex16
  end interface
  interface MPI_Irecv
     module procedure MPI_Irecv_int
     module procedure MPI_Irecv_real8
     module procedure MPI_Irecv_complex16
  end interface
  interface MPI_Bcast
     module procedure MPI_Bcast_int
     module procedure MPI_Bcast_real8
     module procedure MPI_Bcast_complex16
  end interface
  interface MPI_Reduce
     module procedure MPI_Reduce_int
     module procedure MPI_Reduce_int_array
     module procedure MPI_Reduce_real8
     module procedure MPI_Reduce_real8_array
     module procedure MPI_Reduce_complex16
     module procedure MPI_Reduce_complex16_array
  end interface
  interface MPI_AllReduce
     module procedure MPI_AllReduce_int
     module procedure MPI_AllReduce_real8
     module procedure MPI_AllReduce_complex16
  end interface
contains
  subroutine MPI_init(ec)
    integer :: ec
  end subroutine MPI_init
  subroutine MPI_finalize(ec)
    integer :: ec
  end subroutine MPI_finalize
  subroutine MPI_comm_size(comm, np, ec)
    integer :: comm, np, ec
    np = 1
  end subroutine MPI_comm_size
  subroutine MPI_comm_rank(comm, id, ec)
    integer :: comm, id, ec
    id = 0
  end subroutine MPI_comm_rank
  subroutine MPI_type_contiguous(num, old_type, new_type, ec)
    integer :: num, old_type, new_type, ec
  end subroutine MPI_type_contiguous
  subroutine MPI_type_commit(type, ec)
    integer :: type, ec
  end subroutine MPI_type_commit
  subroutine MPI_barrier(comm, ec)
    integer :: comm, ec
  end subroutine MPI_barrier
  
  subroutine MPI_Isend_int(buf, num, type, dest, tag, comm, request, ec)
    integer :: buf, num, type, dest, tag, comm, request, ec
  end subroutine MPI_Isend_int
  subroutine MPI_Isend_real8(buf, num, type, dest, tag, comm, request, ec)
    integer :: num, type, dest, tag, comm, request, ec
    real*8 :: buf
  end subroutine MPI_Isend_real8
  subroutine MPI_Isend_complex16(buf, num, type, dest, tag, comm, request, ec)
    integer :: num, type, dest, tag, comm, request, ec
    complex*16 :: buf
  end subroutine MPI_Isend_complex16
  
  subroutine MPI_Irecv_int(buf, num, type, source, tag, comm, request, ec)
    integer :: buf, num, type, source, tag, comm, request, ec
  end subroutine MPI_Irecv_int
  subroutine MPI_Irecv_real8(buf, num, type, source, tag, comm, request, ec)
    integer :: num, type, source, tag, comm, request, ec
    real*8 :: buf
  end subroutine MPI_Irecv_real8
  subroutine MPI_Irecv_complex16(buf, num, type, source, tag, comm, request, ec)
    integer :: num, type, source, tag, comm, request, ec
    complex*16 :: buf
  end subroutine MPI_Irecv_complex16
  
  subroutine MPI_Waitall(num, request, status, ec)
    integer :: num, request(:), status(:,:), ec
  end subroutine MPI_Waitall
  
  subroutine MPI_Reduce_int(source, buf, num, type, cal_type, root, comm, ec)
    integer :: source, buf
    integer :: num, type, cal_type, root, comm, ec
    buf = source
  end subroutine MPI_Reduce_int
  subroutine MPI_Reduce_real8(source, buf, num, type, cal_type, root, comm, ec)
    real*8 :: source, buf
    integer :: num, type, cal_type, root, comm, ec
    buf = source
  end subroutine MPI_Reduce_real8
  subroutine MPI_Reduce_complex16(source, buf, num, type, cal_type, root, comm, ec)
    complex*16 :: source, buf
    integer :: num, type, cal_type, root, comm, ec
    buf = source
  end subroutine MPI_Reduce_complex16
  subroutine MPI_Reduce_int_array(source, buf, num, type, cal_type, root, comm, ec)
    integer, dimension(:) :: source, buf
    integer :: num, type, cal_type, root, comm, ec
    buf(1:num) = source(1:num)
  end subroutine MPI_Reduce_int_array
  subroutine MPI_Reduce_real8_array(source, buf, num, type, cal_type, root, comm, ec)
    real*8, dimension(:) :: source, buf
    integer :: num, type, cal_type, root, comm, ec
    buf(1:num) = source(1:num)
  end subroutine MPI_Reduce_real8_array
  subroutine MPI_Reduce_complex16_array(source, buf, num, type, cal_type, root, comm, ec)
    complex*16, dimension(:) :: source, buf
    integer :: num, type, cal_type, root, comm, ec
    buf(1:num) = source(1:num)
  end subroutine MPI_Reduce_complex16_array
  
  subroutine MPI_AllReduce_int(source, buf, num, type, cal_type, comm, ec)
    integer :: source, buf, num, type, cal_type, comm, ec
  end subroutine MPI_AllReduce_int
  subroutine MPI_AllReduce_real8(source, buf, num, type, cal_type, comm, ec)
    integer :: num, type, cal_type, comm, ec
    real*8 :: source, buf
  end subroutine MPI_AllReduce_real8
  subroutine MPI_AllReduce_complex16(source, buf, num, type, cal_type, comm, ec)
    integer :: num, type, cal_type, comm, ec
    complex*16 :: source, buf
  end subroutine MPI_AllReduce_complex16
  
  subroutine MPI_Bcast_int(source, num, type, root, comm, ec)
    integer :: source, num, type, root, comm, ec
  end subroutine MPI_Bcast_int
  subroutine MPI_Bcast_real8(source, num, type, root, comm, ec)
    integer :: num, type, root, comm, ec
    real*8 :: source
  end subroutine MPI_Bcast_real8
  subroutine MPI_Bcast_complex16(source, num, type, root, comm, ec)
    integer :: num, type, root, comm, ec
    complex*16 :: source
  end subroutine MPI_Bcast_complex16
end module MPI_dummy
