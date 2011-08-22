! Used Bitmap: 'dualBase_downSize_22x14.bmp' & ''
! epsR: 10.2, freq: 2000000000.0, tanD: 0.005, substrateDepth: 0.0006
!ARfdtd modelinged by sugimoto modeling tool
include 'MPI_dummy.f90'
!module setup
!implicit none
!include 'mpif.h'
module setup
  use MPI_dummy
  implicit none
  ! Constant
  ! Don't touch without ZL.
  ! (ZL is characteristic impedance on transmission line.)
  real*8, parameter :: pi = 3.1415926535897932384626433832795028841971693993751d0
  real*8, parameter :: DBL_EPSILON = 1.0d-5
  real*8, parameter :: c = 2.99792458d8
  real*8, parameter :: mu0 = 4.0d0*pi*1.0d-7
  real*8, parameter :: eps0 = 1.0d0/(mu0*c*c)
  real*8, parameter :: sgm0 = 0.0d0
  real*8, parameter :: Z0 = 120.0d0*pi
  real*8, parameter :: ZL = 50.0d0
  ! File name
  character*128, parameter :: filename='dualBase_downSize_22x14'
  ! FDTD setup
  integer, parameter :: nx=300
  integer, parameter :: ny=300
  integer, parameter :: nz=100
  integer, parameter :: nt=20000
  integer, parameter :: nfeed=4
  real*8, parameter :: dx=0.1d-3
  real*8, parameter :: dy=0.1d-3
  real*8, parameter :: dz=0.1d-3
  real*8, parameter :: CFL=0.9995d0
  ! Modeling Mode(unpopulated)
  ! 0 : Arakawa Engine
  ! 1 : ASoC-compatible Engine
  integer, parameter :: ModelingMode=0
  ! PML setup
  integer, parameter :: LM=8
  integer, parameter :: MM=4
  real*8, parameter :: RC0 = -120.0d0
  ! impedance setup
  ! if you need not impedance data, please set impedanceFlag to 0.
  real*8, parameter :: freqS_imp = 500.0d6
  real*8, parameter :: freqE_imp = 10.0d9
  integer, parameter :: freqPoint_imp = 301
  integer, parameter :: impedanceFlag = 1
  ! pattern setup
  ! If you need not pattern data, please set patternFlag to 0.
  ! If you need not efficiency data, please set efficiencyFlag to 0.
  integer, parameter :: nAngle = 180
  real*8, parameter :: freqS_pat = 0
  real*8, parameter :: freqE_pat = 1.0d9
  integer, parameter :: freqPoint_pat = 2
  integer, parameter :: XS_pat = 0, YS_pat = 0, ZS_pat = 0
  integer, parameter :: XE_pat = 10, YE_pat = 10, ZE_pat = 10
  integer, parameter :: patternFlag = 0
  integer, parameter :: efficiencyFlag = 0
  ! Current distribution
  ! XS_CD == XE_CD : Y-Z plane
  ! YS_CD == YE_CD : X-Z plane
  ! ZS_CD == ZE_CD : X-Y plane
  integer, parameter :: CDFlag = 0
  integer, parameter :: XS_CD = 0, YS_CD = 0, ZS_CD = 0
  integer, parameter :: XE_CD = 10, YE_CD = 10, ZE_CD = 10
  real*8, parameter :: freqS_CD = 0d9
  real*8, parameter :: freqE_CD = 1d9
  integer, parameter :: freqPoint_CD = 2
  complex*16, dimension(:,:,:), allocatable :: Jx, Jy, Jz
  complex*16, dimension(:,:), allocatable :: Jl
  integer :: ZSL_CD, ZEL_CD
  ! Electric field distribution
  ! XS_ED == XE_ED : Y-Z plane
  ! YS_ED == YE_ED : X-Z plan
  ! ZS_ED == ZE_ED : X-Y plane
  integer, parameter :: EDFlag = 0
  integer, parameter :: XS_ED = 0, YS_ED = 0, ZS_ED = 0
  integer, parameter :: XE_ED = 10, YE_ED = 10, ZE_ED = 10
  real*8, parameter :: freqS_ED = 3.0d9
  real*8, parameter :: freqE_ED = 11.0d9
  integer, parameter :: freqPoint_ED = 1
  complex*16, dimension(:,:,:), allocatable :: Ex_ED, Ey_ED, Ez_ED
  integer :: ZSL_ED, ZEL_ED
  ! PLRC setup
  ! If you don't use PLRC-method set PLRCFlag to 0.
  integer, parameter :: XS_RC = 10, YS_RC = 10, ZS_RC = 10
  integer, parameter :: XE_RC = 90, YE_RC = 90, ZE_RC = 90
  integer, parameter :: PLRCFlag = 0
  ! CP setup
  ! If you don't use CP-FDTD, please set CPFlag to 0.
  integer, parameter :: CPFlag = 0
  ! ID setup
  integer, parameter :: ListMax = 128
  ! dt, df
  real*8 :: dt, df
  ! E-field array
  real*8, dimension(:,:,:), allocatable :: Ex,Ey,Ez
  real*8, dimension(:,:,:), allocatable :: Exy1,Exz1, Eyx1,Eyz1, Ezx1,Ezy1
  real*8, dimension(:,:,:), allocatable :: Exy2,Exz2, Eyx2,Eyz2, Ezx2,Ezy2
  real*8, dimension(:,:,:), allocatable :: Exy3,Exz3, Eyx3,Eyz3, Ezx3,Ezy3
  real*8, dimension(:,:,:), allocatable :: Exy4,Exz4, Eyx4,Eyz4, Ezx4,Ezy4
  real*8, dimension(:,:,:), allocatable :: Exy5,Exz5, Eyx5,Eyz5, Ezx5,Ezy5
  real*8, dimension(:,:,:), allocatable :: Exy6,Exz6, Eyx6,Eyz6, Ezx6,Ezy6
  ! H-field array
  real*8, dimension(:,:,:), allocatable :: Hx,Hy,Hz
  real*8, dimension(:,:,:), allocatable :: Hxy1,Hxz1, Hyx1,Hyz1, Hzx1,Hzy1
  real*8, dimension(:,:,:), allocatable :: Hxy2,Hxz2, Hyx2,Hyz2, Hzx2,Hzy2
  real*8, dimension(:,:,:), allocatable :: Hxy3,Hxz3, Hyx3,Hyz3, Hzx3,Hzy3
  real*8, dimension(:,:,:), allocatable :: Hxy4,Hxz4, Hyx4,Hyz4, Hzx4,Hzy4
  real*8, dimension(:,:,:), allocatable :: Hxy5,Hxz5, Hyx5,Hyz5, Hzx5,Hzy5
  real*8, dimension(:,:,:), allocatable :: Hxy6,Hxz6, Hyx6,Hyz6, Hzx6,Hzy6
  ! Material array
  integer, dimension(:,:,:), allocatable :: IDEx,IDEy,IDEz
  integer, dimension(:,:,:), allocatable :: IDHx,IDHy,IDHz
  real*8 :: CEx(-1:ListMax+1), CExLy(-1:ListMax+1), CExLz(-1:ListMax+1)
  real*8 :: CEy(-1:ListMax+1), CEyLz(-1:ListMax+1), CEyLx(-1:ListMax+1)
  real*8 :: CEz(-1:ListMax+1), CEzLx(-1:ListMax+1), CEzLy(-1:ListMax+1)
  real*8 :: CPhi(-1:ListMax+1)
  real*8 :: CPhi1(-1:ListMax+1), CPhi2(-1:ListMax+1), CPhi3(-1:ListMax+1)
  real*8 :: CHxLy(0:ListMax+1), CHxLz(0:ListMax+1)
  real*8 :: CHyLz(0:ListMax+1), CHyLx(0:ListMax+1)
  real*8 :: CHzLx(0:ListMax+1), CHzLy(0:ListMax+1)
  real*8 :: CpmlEdx(1:nx), CpmlEdy(1:ny), CpmlEdz(1:nz)
  real*8 :: CpmlEdxL(1:nx), CpmlEdyL(1:ny), CpmlEdzL(1:nz)
  real*8 :: CpmlHdx(1:nx), CpmlHdy(1:ny), CpmlHdz(1:nz)
  real*8 :: CpmlHdxL(1:nx), CpmlHdyL(1:ny), CpmlHdzL(1:nz)
  ! feed array
  integer :: feedx(nfeed), feedy(nfeed), feedz(nfeed)
  integer :: feedcx(nfeed), feedcy(nfeed), feedcz(nfeed), feedNode(nfeed)
  integer :: feedLength(nfeed)
  character :: feedAxis(nfeed)
  integer :: feedSign(nfeed)
  real*8 :: feedR(nfeed)
  integer :: feedType(nfeed)
  real*8 :: feedTAU(nfeed), feedOMEGA(nfeed), feedALPHA(nfeed)
  real*8 :: feedAMP(nfeed)
  real*8 ::feed
  ! impedance array
  real*8 :: It(nfeed), Vt(nfeed)
  complex*16 :: If_imp(freqPoint_imp, nfeed), Vf_imp(freqPoint_imp, nfeed)
  complex*16 :: imp(freqPoint_imp, nfeed)
  ! pattern array
  complex*16, dimension(:,:,:), allocatable :: Ey1, Ez1, Hy1, Hz1
  complex*16, dimension(:,:,:), allocatable :: Ey2, Ez2, Hy2, Hz2
  complex*16, dimension(:,:,:), allocatable :: Ez3, Ex3, Hz3, Hx3
  complex*16, dimension(:,:,:), allocatable :: Ez4, Ex4, Hz4, Hx4
  complex*16, dimension(:,:,:), allocatable :: Ex5, Ey5, Hx5, Hy5
  complex*16, dimension(:,:,:), allocatable :: Ex6, Ey6, Hx6, Hy6
  complex*16 :: If_pat(freqPoint_pat, nfeed), Vf_pat(freqPoint_pat, nfeed)
  real*8 :: Pin(freqPoint_pat)
  real*8 :: Prad(freqPoint_pat)
  integer :: ZSL_pat, ZEL_pat
  real*8 :: XC_pat, YC_pat, ZC_pat
  ! PLRC array
  real*8, dimension(:,:,:), allocatable :: ExSaveRC, EySaveRC, EzSaveRC
  real*8, dimension(:,:,:), allocatable :: PHIx, PHIy, PHIz
  integer :: ZSL_RC, ZEL_RC
  ! CP array
  integer, save :: cpPoint = 0
  integer, dimension(:), allocatable :: cpX, cpY, cpZ, cpPlaneFlag
  real*8, dimension(:), allocatable :: llx1,lly1,llz1, llx2,lly2,llz2
  real*8, dimension(:), allocatable :: sq, CHCP
  integer :: cpS(1:nz), cpE(1:nz)
  ! roop counter
  integer :: t
  ! MPI parameter
  integer :: np, id
  integer :: zm, zp
  integer :: tag = 0
  integer :: status(MPI_STATUS_SIZE, 4)
  integer :: request(4)
  integer :: error_code
  integer :: MatrixXY
  ! Load barancing parameter
  integer, dimension(:), allocatable :: nzS, nzE, nzL
  ! Avoid from compiler's alart
  real*8 :: trush
  ! interface
  interface setCP
     module procedure setCP_integer
     module procedure setCP_real
  end interface
  interface setline
     module procedure ARsetline
     module procedure ASoCsetline
  end interface
  interface setplane
     module procedure ARsetplane
     module procedure ASoCsetplane
  end interface
  interface setfeed
     module procedure ARsetfeed
     module procedure ASoCsetfeed
  end interface
contains
  subroutine beforeInit
  end subroutine beforeInit

  subroutine setmodel !------------------------------------------------------------
    ! setfeed(feedNo, x1,y1,z1, x2,y2,z2, fR, fType, fTAU, fAMP)
    ! setline(materialID, x1,y1,z1, x2,y2,z2)
    ! setplane(materialID, x1,y1,z1, x2,y2,z2)
    ! setbox(materialID, x1,y1,z1, x2,y2,z2)
    ! setoval(materialID, x1,y1,z1, x2,y2,z2)
    ! setovalCP(materialID, x1,y1,z1, x2,y2,z2)
   !-------------------------------------!
   !          ID    SIG    EPR           !
   !-------------------------------------!


    call medium(2, 5.760d7, 1.0d0)  !Cupper
    call medium(3, 0.005674515286333190d0, 10.20d0)  !Substrate
    call medium(4, 0.00283725764316660d0, 5.60d0)  !surface

    call setbox(3, 40, 80, 47, 260, 220, 53)
    call setplane(4, 40, 80, 47,   260, 220, 47)
    call setplane(4, 40, 80, 53,   260, 220, 53)
    call setplane(2, 40, 80, 53,   260, 81, 53)
    call setplane(2, 40, 81, 53,   260, 82, 53)
    call setplane(2, 40, 82, 53,   260, 83, 53)
    call setplane(2, 40, 83, 53,   260, 84, 53)
    call setplane(2, 40, 84, 53,   260, 85, 53)
    call setplane(2, 40, 85, 53,   260, 86, 53)
    call setplane(2, 40, 86, 53,   260, 87, 53)
    call setplane(2, 40, 87, 53,   260, 88, 53)
    call setplane(2, 40, 88, 53,   260, 89, 53)
    call setplane(2, 40, 89, 53,   260, 90, 53)
    call setplane(2, 40, 90, 53,   260, 91, 53)
    call setplane(2, 40, 91, 53,   260, 92, 53)
    call setplane(2, 40, 92, 53,   260, 93, 53)
    call setplane(2, 40, 93, 53,   260, 94, 53)
    call setplane(2, 40, 94, 53,   260, 95, 53)
    call setplane(2, 40, 95, 53,   260, 96, 53)
    call setplane(2, 40, 96, 53,   260, 97, 53)
    call setplane(2, 40, 97, 53,   260, 98, 53)
    call setplane(2, 40, 98, 53,   260, 99, 53)
    call setplane(2, 40, 99, 53,   260, 100, 53)
    call setplane(2, 40, 100, 53,   110, 101, 53)
    call setplane(2, 141, 100, 53,   159, 101, 53)
    call setplane(2, 190, 100, 53,   260, 101, 53)
    call setplane(2, 40, 101, 53,   110, 102, 53)
    call setplane(2, 141, 101, 53,   159, 102, 53)
    call setplane(2, 190, 101, 53,   260, 102, 53)
    call setplane(2, 40, 102, 53,   139, 103, 53)
    call setplane(2, 141, 102, 53,   159, 103, 53)
    call setplane(2, 161, 102, 53,   260, 103, 53)
    call setplane(2, 40, 103, 53,   139, 104, 53)
    call setplane(2, 141, 103, 53,   159, 104, 53)
    call setplane(2, 161, 103, 53,   260, 104, 53)
    call setplane(2, 40, 104, 53,   110, 105, 53)
    call setplane(2, 141, 104, 53,   159, 105, 53)
    call setplane(2, 190, 104, 53,   260, 105, 53)
    call setplane(2, 40, 105, 53,   110, 106, 53)
    call setplane(2, 141, 105, 53,   159, 106, 53)
    call setplane(2, 190, 105, 53,   260, 106, 53)
    call setplane(2, 40, 106, 53,   110, 107, 53)
    call setplane(2, 112, 106, 53,   188, 107, 53)
    call setplane(2, 190, 106, 53,   260, 107, 53)
    call setplane(2, 40, 107, 53,   110, 108, 53)
    call setplane(2, 112, 107, 53,   188, 108, 53)
    call setplane(2, 190, 107, 53,   260, 108, 53)
    call setplane(2, 40, 108, 53,   110, 109, 53)
    call setplane(2, 141, 108, 53,   159, 109, 53)
    call setplane(2, 190, 108, 53,   260, 109, 53)
    call setplane(2, 40, 109, 53,   110, 110, 53)
    call setplane(2, 141, 109, 53,   159, 110, 53)
    call setplane(2, 190, 109, 53,   260, 110, 53)
    call setplane(2, 40, 110, 53,   139, 111, 53)
    call setplane(2, 141, 110, 53,   159, 111, 53)
    call setplane(2, 161, 110, 53,   260, 111, 53)
    call setplane(2, 40, 111, 53,   139, 112, 53)
    call setplane(2, 141, 111, 53,   159, 112, 53)
    call setplane(2, 161, 111, 53,   260, 112, 53)
    call setplane(2, 40, 112, 53,   110, 113, 53)
    call setplane(2, 141, 112, 53,   159, 113, 53)
    call setplane(2, 190, 112, 53,   260, 113, 53)
    call setplane(2, 40, 113, 53,   110, 114, 53)
    call setplane(2, 141, 113, 53,   159, 114, 53)
    call setplane(2, 190, 113, 53,   260, 114, 53)
    call setplane(2, 40, 114, 53,   110, 115, 53)
    call setplane(2, 112, 114, 53,   188, 115, 53)
    call setplane(2, 190, 114, 53,   260, 115, 53)
    call setplane(2, 40, 115, 53,   110, 116, 53)
    call setplane(2, 112, 115, 53,   188, 116, 53)
    call setplane(2, 190, 115, 53,   260, 116, 53)
    call setplane(2, 40, 116, 53,   110, 117, 53)
    call setplane(2, 132, 116, 53,   168, 117, 53)
    call setplane(2, 190, 116, 53,   260, 117, 53)
    call setplane(2, 40, 117, 53,   110, 118, 53)
    call setplane(2, 132, 117, 53,   168, 118, 53)
    call setplane(2, 190, 117, 53,   260, 118, 53)
    call setplane(2, 40, 118, 53,   130, 119, 53)
    call setplane(2, 132, 118, 53,   168, 119, 53)
    call setplane(2, 170, 118, 53,   260, 119, 53)
    call setplane(2, 40, 119, 53,   130, 120, 53)
    call setplane(2, 132, 119, 53,   168, 120, 53)
    call setplane(2, 170, 119, 53,   260, 120, 53)
    call setplane(2, 40, 120, 53,   110, 121, 53)
    call setplane(2, 145, 120, 53,   155, 121, 53)
    call setplane(2, 190, 120, 53,   260, 121, 53)
    call setplane(2, 40, 121, 53,   110, 122, 53)
    call setplane(2, 145, 121, 53,   155, 122, 53)
    call setplane(2, 190, 121, 53,   260, 122, 53)
    call setplane(2, 40, 122, 53,   110, 123, 53)
    call setplane(2, 112, 122, 53,   143, 123, 53)
    call setplane(2, 145, 122, 53,   155, 123, 53)
    call setplane(2, 157, 122, 53,   188, 123, 53)
    call setplane(2, 190, 122, 53,   260, 123, 53)
    call setplane(2, 40, 123, 53,   110, 124, 53)
    call setplane(2, 112, 123, 53,   143, 124, 53)
    call setplane(2, 145, 123, 53,   155, 124, 53)
    call setplane(2, 157, 123, 53,   188, 124, 53)
    call setplane(2, 190, 123, 53,   260, 124, 53)
    call setplane(2, 40, 124, 53,   110, 125, 53)
    call setplane(2, 135, 124, 53,   143, 125, 53)
    call setplane(2, 145, 124, 53,   155, 125, 53)
    call setplane(2, 157, 124, 53,   165, 125, 53)
    call setplane(2, 190, 124, 53,   260, 125, 53)
    call setplane(2, 40, 125, 53,   110, 126, 53)
    call setplane(2, 135, 125, 53,   138, 126, 53)
    call setplane(2, 140, 125, 53,   143, 126, 53)
    call setplane(2, 145, 125, 53,   155, 126, 53)
    call setplane(2, 157, 125, 53,   160, 126, 53)
    call setplane(2, 162, 125, 53,   165, 126, 53)
    call setplane(2, 190, 125, 53,   260, 126, 53)
    call setplane(2, 40, 126, 53,   133, 127, 53)
    call setplane(2, 135, 126, 53,   138, 127, 53)
    call setplane(2, 140, 126, 53,   143, 127, 53)
    call setplane(2, 145, 126, 53,   155, 127, 53)
    call setplane(2, 157, 126, 53,   160, 127, 53)
    call setplane(2, 162, 126, 53,   165, 127, 53)
    call setplane(2, 167, 126, 53,   260, 127, 53)
    call setplane(2, 40, 127, 53,   133, 128, 53)
    call setplane(2, 135, 127, 53,   138, 128, 53)
    call setplane(2, 140, 127, 53,   143, 128, 53)
    call setplane(2, 145, 127, 53,   155, 128, 53)
    call setplane(2, 157, 127, 53,   160, 128, 53)
    call setplane(2, 162, 127, 53,   165, 128, 53)
    call setplane(2, 167, 127, 53,   260, 128, 53)
    call setplane(2, 40, 128, 53,   110, 129, 53)
    call setplane(2, 135, 128, 53,   138, 129, 53)
    call setplane(2, 140, 128, 53,   143, 129, 53)
    call setplane(2, 145, 128, 53,   155, 129, 53)
    call setplane(2, 157, 128, 53,   160, 129, 53)
    call setplane(2, 162, 128, 53,   165, 129, 53)
    call setplane(2, 190, 128, 53,   260, 129, 53)
    call setplane(2, 40, 129, 53,   110, 130, 53)
    call setplane(2, 135, 129, 53,   138, 130, 53)
    call setplane(2, 145, 129, 53,   155, 130, 53)
    call setplane(2, 162, 129, 53,   165, 130, 53)
    call setplane(2, 190, 129, 53,   260, 130, 53)
    call setplane(2, 40, 130, 53,   110, 131, 53)
    call setplane(2, 112, 130, 53,   138, 131, 53)
    call setplane(2, 145, 130, 53,   155, 131, 53)
    call setplane(2, 162, 130, 53,   188, 131, 53)
    call setplane(2, 190, 130, 53,   260, 131, 53)
    call setplane(2, 40, 131, 53,   110, 132, 53)
    call setplane(2, 112, 131, 53,   188, 132, 53)
    call setplane(2, 190, 131, 53,   260, 132, 53)
    call setplane(2, 40, 132, 53,   110, 133, 53)
    call setplane(2, 112, 132, 53,   188, 133, 53)
    call setplane(2, 190, 132, 53,   260, 133, 53)
    call setplane(2, 40, 133, 53,   110, 134, 53)
    call setplane(2, 112, 133, 53,   188, 134, 53)
    call setplane(2, 190, 133, 53,   260, 134, 53)
    call setplane(2, 40, 140, 53,   142, 141, 53)
    call setplane(2, 158, 140, 53,   260, 141, 53)
    call setplane(2, 40, 141, 53,   142, 142, 53)
    call setplane(2, 158, 141, 53,   260, 142, 53)
    call setplane(2, 40, 142, 53,   142, 143, 53)
    call setplane(2, 158, 142, 53,   260, 143, 53)
    call setplane(2, 40, 143, 53,   142, 144, 53)
    call setplane(2, 158, 143, 53,   260, 144, 53)
    call setplane(2, 40, 144, 53,   142, 145, 53)
    call setplane(2, 158, 144, 53,   260, 145, 53)
    call setplane(2, 40, 145, 53,   142, 146, 53)
    call setplane(2, 158, 145, 53,   260, 146, 53)
    call setplane(2, 40, 146, 53,   142, 147, 53)
    call setplane(2, 158, 146, 53,   260, 147, 53)
    call setplane(2, 40, 147, 53,   142, 148, 53)
    call setplane(2, 158, 147, 53,   260, 148, 53)
    call setplane(2, 40, 148, 53,   142, 149, 53)
    call setplane(2, 158, 148, 53,   260, 149, 53)
    call setplane(2, 40, 149, 53,   142, 150, 53)
    call setplane(2, 158, 149, 53,   260, 150, 53)
    call setplane(2, 40, 150, 53,   142, 151, 53)
    call setplane(2, 158, 150, 53,   260, 151, 53)
    call setplane(2, 40, 151, 53,   142, 152, 53)
    call setplane(2, 158, 151, 53,   260, 152, 53)
    call setplane(2, 40, 152, 53,   142, 153, 53)
    call setplane(2, 158, 152, 53,   260, 153, 53)
    call setplane(2, 40, 153, 53,   142, 154, 53)
    call setplane(2, 158, 153, 53,   260, 154, 53)
    call setplane(2, 40, 154, 53,   142, 155, 53)
    call setplane(2, 158, 154, 53,   260, 155, 53)
    call setplane(2, 40, 155, 53,   142, 156, 53)
    call setplane(2, 158, 155, 53,   260, 156, 53)
    call setplane(2, 40, 156, 53,   142, 157, 53)
    call setplane(2, 158, 156, 53,   260, 157, 53)
    call setplane(2, 40, 157, 53,   142, 158, 53)
    call setplane(2, 158, 157, 53,   260, 158, 53)
    call setplane(2, 40, 158, 53,   142, 159, 53)
    call setplane(2, 158, 158, 53,   260, 159, 53)
    call setplane(2, 40, 159, 53,   142, 160, 53)
    call setplane(2, 158, 159, 53,   260, 160, 53)
    call setplane(2, 40, 166, 53,   110, 167, 53)
    call setplane(2, 112, 166, 53,   188, 167, 53)
    call setplane(2, 190, 166, 53,   260, 167, 53)
    call setplane(2, 40, 167, 53,   110, 168, 53)
    call setplane(2, 112, 167, 53,   188, 168, 53)
    call setplane(2, 190, 167, 53,   260, 168, 53)
    call setplane(2, 40, 168, 53,   110, 169, 53)
    call setplane(2, 112, 168, 53,   188, 169, 53)
    call setplane(2, 190, 168, 53,   260, 169, 53)
    call setplane(2, 40, 169, 53,   110, 170, 53)
    call setplane(2, 112, 169, 53,   138, 170, 53)
    call setplane(2, 145, 169, 53,   155, 170, 53)
    call setplane(2, 162, 169, 53,   188, 170, 53)
    call setplane(2, 190, 169, 53,   260, 170, 53)
    call setplane(2, 40, 170, 53,   110, 171, 53)
    call setplane(2, 135, 170, 53,   138, 171, 53)
    call setplane(2, 145, 170, 53,   155, 171, 53)
    call setplane(2, 162, 170, 53,   165, 171, 53)
    call setplane(2, 190, 170, 53,   260, 171, 53)
    call setplane(2, 40, 171, 53,   110, 172, 53)
    call setplane(2, 135, 171, 53,   138, 172, 53)
    call setplane(2, 140, 171, 53,   143, 172, 53)
    call setplane(2, 145, 171, 53,   155, 172, 53)
    call setplane(2, 157, 171, 53,   160, 172, 53)
    call setplane(2, 162, 171, 53,   165, 172, 53)
    call setplane(2, 190, 171, 53,   260, 172, 53)
    call setplane(2, 40, 172, 53,   133, 173, 53)
    call setplane(2, 135, 172, 53,   138, 173, 53)
    call setplane(2, 140, 172, 53,   143, 173, 53)
    call setplane(2, 145, 172, 53,   155, 173, 53)
    call setplane(2, 157, 172, 53,   160, 173, 53)
    call setplane(2, 162, 172, 53,   165, 173, 53)
    call setplane(2, 167, 172, 53,   260, 173, 53)
    call setplane(2, 40, 173, 53,   133, 174, 53)
    call setplane(2, 135, 173, 53,   138, 174, 53)
    call setplane(2, 140, 173, 53,   143, 174, 53)
    call setplane(2, 145, 173, 53,   155, 174, 53)
    call setplane(2, 157, 173, 53,   160, 174, 53)
    call setplane(2, 162, 173, 53,   165, 174, 53)
    call setplane(2, 167, 173, 53,   260, 174, 53)
    call setplane(2, 40, 174, 53,   110, 175, 53)
    call setplane(2, 135, 174, 53,   138, 175, 53)
    call setplane(2, 140, 174, 53,   143, 175, 53)
    call setplane(2, 145, 174, 53,   155, 175, 53)
    call setplane(2, 157, 174, 53,   160, 175, 53)
    call setplane(2, 162, 174, 53,   165, 175, 53)
    call setplane(2, 190, 174, 53,   260, 175, 53)
    call setplane(2, 40, 175, 53,   110, 176, 53)
    call setplane(2, 135, 175, 53,   143, 176, 53)
    call setplane(2, 145, 175, 53,   155, 176, 53)
    call setplane(2, 157, 175, 53,   165, 176, 53)
    call setplane(2, 190, 175, 53,   260, 176, 53)
    call setplane(2, 40, 176, 53,   110, 177, 53)
    call setplane(2, 112, 176, 53,   143, 177, 53)
    call setplane(2, 145, 176, 53,   155, 177, 53)
    call setplane(2, 157, 176, 53,   188, 177, 53)
    call setplane(2, 190, 176, 53,   260, 177, 53)
    call setplane(2, 40, 177, 53,   110, 178, 53)
    call setplane(2, 112, 177, 53,   143, 178, 53)
    call setplane(2, 145, 177, 53,   155, 178, 53)
    call setplane(2, 157, 177, 53,   188, 178, 53)
    call setplane(2, 190, 177, 53,   260, 178, 53)
    call setplane(2, 40, 178, 53,   110, 179, 53)
    call setplane(2, 145, 178, 53,   155, 179, 53)
    call setplane(2, 190, 178, 53,   260, 179, 53)
    call setplane(2, 40, 179, 53,   110, 180, 53)
    call setplane(2, 145, 179, 53,   155, 180, 53)
    call setplane(2, 190, 179, 53,   260, 180, 53)
    call setplane(2, 40, 180, 53,   130, 181, 53)
    call setplane(2, 132, 180, 53,   168, 181, 53)
    call setplane(2, 170, 180, 53,   260, 181, 53)
    call setplane(2, 40, 181, 53,   130, 182, 53)
    call setplane(2, 132, 181, 53,   168, 182, 53)
    call setplane(2, 170, 181, 53,   260, 182, 53)
    call setplane(2, 40, 182, 53,   110, 183, 53)
    call setplane(2, 132, 182, 53,   168, 183, 53)
    call setplane(2, 190, 182, 53,   260, 183, 53)
    call setplane(2, 40, 183, 53,   110, 184, 53)
    call setplane(2, 132, 183, 53,   168, 184, 53)
    call setplane(2, 190, 183, 53,   260, 184, 53)
    call setplane(2, 40, 184, 53,   110, 185, 53)
    call setplane(2, 112, 184, 53,   188, 185, 53)
    call setplane(2, 190, 184, 53,   260, 185, 53)
    call setplane(2, 40, 185, 53,   110, 186, 53)
    call setplane(2, 112, 185, 53,   188, 186, 53)
    call setplane(2, 190, 185, 53,   260, 186, 53)
    call setplane(2, 40, 186, 53,   110, 187, 53)
    call setplane(2, 141, 186, 53,   159, 187, 53)
    call setplane(2, 190, 186, 53,   260, 187, 53)
    call setplane(2, 40, 187, 53,   110, 188, 53)
    call setplane(2, 141, 187, 53,   159, 188, 53)
    call setplane(2, 190, 187, 53,   260, 188, 53)
    call setplane(2, 40, 188, 53,   139, 189, 53)
    call setplane(2, 141, 188, 53,   159, 189, 53)
    call setplane(2, 161, 188, 53,   260, 189, 53)
    call setplane(2, 40, 189, 53,   139, 190, 53)
    call setplane(2, 141, 189, 53,   159, 190, 53)
    call setplane(2, 161, 189, 53,   260, 190, 53)
    call setplane(2, 40, 190, 53,   110, 191, 53)
    call setplane(2, 141, 190, 53,   159, 191, 53)
    call setplane(2, 190, 190, 53,   260, 191, 53)
    call setplane(2, 40, 191, 53,   110, 192, 53)
    call setplane(2, 141, 191, 53,   159, 192, 53)
    call setplane(2, 190, 191, 53,   260, 192, 53)
    call setplane(2, 40, 192, 53,   110, 193, 53)
    call setplane(2, 112, 192, 53,   188, 193, 53)
    call setplane(2, 190, 192, 53,   260, 193, 53)
    call setplane(2, 40, 193, 53,   110, 194, 53)
    call setplane(2, 112, 193, 53,   188, 194, 53)
    call setplane(2, 190, 193, 53,   260, 194, 53)
    call setplane(2, 40, 194, 53,   110, 195, 53)
    call setplane(2, 141, 194, 53,   159, 195, 53)
    call setplane(2, 190, 194, 53,   260, 195, 53)
    call setplane(2, 40, 195, 53,   110, 196, 53)
    call setplane(2, 141, 195, 53,   159, 196, 53)
    call setplane(2, 190, 195, 53,   260, 196, 53)
    call setplane(2, 40, 196, 53,   139, 197, 53)
    call setplane(2, 141, 196, 53,   159, 197, 53)
    call setplane(2, 161, 196, 53,   260, 197, 53)
    call setplane(2, 40, 197, 53,   139, 198, 53)
    call setplane(2, 141, 197, 53,   159, 198, 53)
    call setplane(2, 161, 197, 53,   260, 198, 53)
    call setplane(2, 40, 198, 53,   110, 199, 53)
    call setplane(2, 141, 198, 53,   159, 199, 53)
    call setplane(2, 190, 198, 53,   260, 199, 53)
    call setplane(2, 40, 199, 53,   110, 200, 53)
    call setplane(2, 141, 199, 53,   159, 200, 53)
    call setplane(2, 190, 199, 53,   260, 200, 53)
    call setplane(2, 40, 200, 53,   260, 201, 53)
    call setplane(2, 40, 201, 53,   260, 202, 53)
    call setplane(2, 40, 202, 53,   260, 203, 53)
    call setplane(2, 40, 203, 53,   260, 204, 53)
    call setplane(2, 40, 204, 53,   260, 205, 53)
    call setplane(2, 40, 205, 53,   260, 206, 53)
    call setplane(2, 40, 206, 53,   260, 207, 53)
    call setplane(2, 40, 207, 53,   260, 208, 53)
    call setplane(2, 40, 208, 53,   260, 209, 53)
    call setplane(2, 40, 209, 53,   260, 210, 53)
    call setplane(2, 40, 210, 53,   260, 211, 53)
    call setplane(2, 40, 211, 53,   260, 212, 53)
    call setplane(2, 40, 212, 53,   260, 213, 53)
    call setplane(2, 40, 213, 53,   260, 214, 53)
    call setplane(2, 40, 214, 53,   260, 215, 53)
    call setplane(2, 40, 215, 53,   260, 216, 53)
    call setplane(2, 40, 216, 53,   260, 217, 53)
    call setplane(2, 40, 217, 53,   260, 218, 53)
    call setplane(2, 40, 218, 53,   260, 219, 53)
    call setplane(2, 40, 219, 53,   260, 220, 53)


    call setfeed(1, 40, 134, 53,   40, 140, 53, 100.0d0, 1, 800.0d0 * dt, 1.0d0)
    call setfeed(2, 40, 166, 53,   40, 160, 53, 100.0d0, 1, 800.0d0 * dt, 1.0d0)
    call setfeed(3, 260, 134, 53,   260, 140, 53, 100.0d0, 1, 800.0d0 * dt, 0.0d0)
    call setfeed(4, 260, 166, 53,   260, 160, 53, 100.0d0, 1, 800.0d0 * dt, 0.0d0)

  end subroutine setmodel

  subroutine setmaterial(materialID, sgm, eps, mu)
    integer :: materialID
    real*8 :: sgm,eps,mu
    if((materialID >= 2) .and. (materialID <= ListMax+1)) then
       CEx(materialID) = (1.0d0-sgm*dt/2.0d0/eps)/(1.0d0+sgm*dt/2.0d0/eps)
       CExLy(materialID) = dt/eps/(1.0d0+sgm*dt/2.0d0/eps)/dy
       CExLz(materialID) = dt/eps/(1.0d0+sgm*dt/2.0d0/eps)/dz
       CEy(materialID) = (1.0d0-sgm*dt/2.0d0/eps)/(1.0d0+sgm*dt/2.0d0/eps)
       CEyLz(materialID) = dt/eps/(1.0d0+sgm*dt/2.0d0/eps)/dz
       CEyLx(materialID) = dt/eps/(1.0d0+sgm*dt/2.0d0/eps)/dx
       CEz(materialID) = (1.0d0-sgm*dt/2.0d0/eps)/(1.0d0+sgm*dt/2.0d0/eps)
       CEzLx(materialID) = dt/eps/(1.0d0+sgm*dt/2.0d0/eps)/dx
       CEzLy(materialID) = dt/eps/(1.0d0+sgm*dt/2.0d0/eps)/dy
    
       CPhi(materialID) = 0.0d0
       CPhi1(materialID) = 0.0d0
       CPhi2(materialID) = 0.0d0
       CPhi3(materialID) = 0.0d0
       
       CHxLy(materialID) = dt/mu/dy
       CHxLz(materialID) = dt/mu/dz
       CHyLz(materialID) = dt/mu/dz
       CHyLx(materialID) = dt/mu/dx
       CHzLx(materialID) = dt/mu/dx
       CHzLy(materialID) = dt/mu/dy
    else if((materialID == 0) .or. (materialID == 1)) then
       stop "Don't use materialID 0 or materialID 1. Error. (setmaterial)"
    endif
  end subroutine setmaterial
  subroutine MEDIUM(materialID, sgm, epsr)
    integer :: materialID
    real*8 :: sgm,epsr
    call setmaterial(materialID, sgm, epsr*eps0, mu0)
  end subroutine MEDIUM
  subroutine setmaterialDebye(materialID, eps_s, eps_inf, t0, mu)
    integer :: materialID
    real*8 :: eps_s, eps_inf, t0, mu
    real*8 :: chi0, dchi0, xi0, dxi0
    
    chi0 = (eps_s - eps_inf)*(1.0d0-exp(-dt/t0))
    dchi0 = chi0 * (1.0d0-exp(-dt/t0))
    xi0 = -(eps_s - eps_inf)*t0/dt*((1.0d0+dt/t0)*exp(-dt/t0)-1.0d0)
    dxi0 = xi0 * (1.0d0-exp(-dt/t0))
    
    CEx(materialID) = (eps_inf - xi0)/(eps_inf + chi0 - xi0)
    CExLy(materialID) = dt/eps0/(eps_inf + chi0 - xi0)/dy
    CExLz(materialID) = dt/eps0/(eps_inf + chi0 - xi0)/dz
    CEy(materialID) = (eps_inf - xi0)/(eps_inf + chi0 - xi0)
    CEyLz(materialID) = dt/eps0/(eps_inf + chi0 - xi0)/dz
    CEyLx(materialID) = dt/eps0/(eps_inf + chi0 - xi0)/dx
    CEz(materialID) = (eps_inf - xi0)/(eps_inf + chi0 - xi0)
    CEzLx(materialID) = dt/eps0/(eps_inf + chi0 - xi0)/dx
    CEzLy(materialID) = dt/eps0/(eps_inf + chi0 - xi0)/dy
    
    CPhi(materialID) = 1.0d0/(eps_inf+chi0-xi0)
    CPhi1(materialID) = dchi0 - dxi0
    CPhi2(materialID) = dxi0
    CPhi3(materialID) = exp(-dt/t0)
    CHxLy(materialID) = dt/mu/dy
    CHxLz(materialID) = dt/mu/dz
    CHyLz(materialID) = dt/mu/dz
    CHyLx(materialID) = dt/mu/dx
    CHzLx(materialID) = dt/mu/dx
    CHzLy(materialID) = dt/mu/dy
  end subroutine setmaterialDebye
  subroutine setResistance(materialID, R)
    integer :: materialID
    real*8 :: R
    
    if(R == 0.0d0) then
       stop 'Error. R = 0. Please use materialID = 1 (PEC).'
    end if
    CEx(materialID) = (1.0d0-dt*dx/2.0d0/R/eps0/dy/dz) &
         & / (1.0d0+dt*dx/2.0d0/R/eps0/dy/dz)
    CExLy(materialID) = dt/eps0&
         & / (1.0d0+dt*dx/2.0d0/R/eps0/dy/dz)/dy
    CExLz(materialID) = dt/eps0&
         & / (1.0d0+dt*dx/2.0d0/R/eps0/dy/dz)/dz
    CEy(materialID) = (1.0d0-dt*dy/2.0d0/R/eps0/dz/dx) &
         & / (1.0d0+dt*dy/2.0d0/R/eps0/dz/dx)
    CEyLz(materialID) = dt/eps0 &
         & / (1.0d0+dt*dy/2.0d0/R/eps0/dz/dx)/dz
    CEyLx(materialID) = dt/eps0 &
         & / (1.0d0+dt*dy/2.0d0/R/eps0/dz/dx)/dx
    CEz(materialID) = (1.0d0-dt*dz/2.0d0/R/eps0/dx/dy) &
         & / (1.0d0+dt*dz/2.0d0/R/eps0/dx/dy)
    CEzLx(materialID) = dt/eps0 &
         & / (1.0d0+dt*dz/2.0d0/R/eps0/dx/dy)/dx
    CEzLy(materialID) = dt/eps0 &
         & / (1.0d0+dt*dz/2.0d0/R/eps0/dx/dy)/dy
    
    CPhi(materialID) = 0.0d0
    CPhi1(materialID) = 0.0d0
    CPhi2(materialID) = 0.0d0
    CPhi3(materialID) = 0.0d0
       
    CHxLy(materialID) = dt/mu0/dy
    CHxLz(materialID) = dt/mu0/dz
    CHyLz(materialID) = dt/mu0/dz
    CHyLx(materialID) = dt/mu0/dx
    CHzLx(materialID) = dt/mu0/dx
    CHzLy(materialID) = dt/mu0/dy
  end subroutine setResistance
  subroutine setCondenser(materialID, Cap)
    integer :: materialID
    real*8 :: Cap
    
    CEx(materialID) = 1.0d0
    CExLy(materialID) = dt/eps0/(1.0d0+Cap*dx/eps0/dy/dz)/dy
    CExLz(materialID) = dt/eps0/(1.0d0+Cap*dx/eps0/dy/dz)/dz
    CEy(materialID) = 1.0d0
    CEyLz(materialID) = dt/eps0/(1.0d0+Cap*dy/eps0/dz/dx)/dz
    CEyLx(materialID) = dt/eps0/(1.0d0+Cap*dy/eps0/dz/dx)/dx
    CEz(materialID) = 1.0d0
    CEzLx(materialID) = dt/eps0/(1.0d0+Cap*dz/eps0/dx/dy)/dx
    CEzLy(materialID) = dt/eps0/(1.0d0+Cap*dz/eps0/dx/dy)/dy
    
    CPhi(materialID) = 0.0d0
    CPhi1(materialID) = 0.0d0
    CPhi2(materialID) = 0.0d0
    CPhi3(materialID) = 0.0d0
       
    CHxLy(materialID) = dt/mu0/dy
    CHxLz(materialID) = dt/mu0/dz
    CHyLz(materialID) = dt/mu0/dz
    CHyLx(materialID) = dt/mu0/dx
    CHzLx(materialID) = dt/mu0/dx
    CHzLy(materialID) = dt/mu0/dy
  end subroutine setCondenser
  
  subroutine ARsetfeed(feedNo, x1,y1,z1, x2,y2,z2, fR, fType, fTAU, fAMP)
    integer :: feedNo
    integer :: x1,y1,z1, x2,y2,z2
    real*8 :: fR
    integer :: fType
    real*8 :: fTAU, fAMP
    integer :: p
    
    feedx(feedNo) = x1
    feedy(feedNo) = y1
    feedz(feedNo) = z1
    feedcx(feedNo) = x1
    feedcy(feedNo) = y1
    feedcz(feedNo) = z1
    feedLength(feedNo) = abs((x2-x1)+(y2-y1)+(z2-z1))
    if(x1 < x2 .and. y1 == y2 .and. z1 == z2) then
       feedAxis(feedNo) = 'x'
       feedSign(feedNo) = +1
       feedx(feedNo) = x1
       feedcx(feedNo) = x1 + (feedLength(feedNo)-1)/2
    else if(x1 > x2 .and. y1 == y2 .and. z1 == z2) then
       feedAxis(feedNo) = 'x'
       feedSign(feedNo) = -1
       feedx(feedNo) = x1-1
       feedcx(feedNo) = x1 - (feedLength(feedNo)+1)/2
    else if(x1 == x2 .and. y1 < y2 .and. z1 == z2) then
       feedAxis(feedNo) = 'y'
       feedSign(feedNo) = +1
       feedy(feedNo) = y1
       feedcy(feedNo) = y1 + (feedLength(feedNo)-1)/2
    else if(x1 == x2 .and. y1 > y2 .and. z1 == z2) then
       feedAxis(feedNo) = 'y'
       feedSign(feedNo) = -1
       feedy(feedNo) = y1-1
       feedcy(feedNo) = y1 - (feedLength(feedNo)+1)/2
    else if(x1 == x2 .and. y1 == y2 .and. z1 < z2) then
       feedAxis(feedNo) = 'z'
       feedSign(feedNo) = +1
       feedz(feedNo) = z1
       feedcz(feedNo) = z1 + (feedLength(feedNo)-1)/2
    else if(x1 == x2 .and. y1 == y2 .and. z1 > z2) then
       feedAxis(feedNo) = 'z'
       feedSign(feedNo) = -1
       feedz(feedNo) = z1-1
       feedcz(feedNo) = z1 - (feedLength(feedNo)+1)/2
    else
       stop 'Error. (setfeed)'
    end if
    do p=0, np-1
       if(nzS(p) <= feedcz(feedNo) .and. feedcz(feedNo) <= nzE(p)) then
          feedNode(feedNo) = p
       end if
    end do
    feedR(feedNo) = fR
    feedType(feedNo) = fType
    feedTAU(feedNo) = fTAU
    feedOMEGA(feedNo) = pi/fTAU
    feedALPHA(feedNo) = (4.0d0/fTAU)**2
    feedAMP(feedNo) = fAMP
  end subroutine ARsetfeed
  subroutine ASoCsetfeed(feedNo, x,y,z, &
       & fLength, fDirection, fR, fType, fTAU, fAMP)
    integer :: feedNo, x,y,z, fLength,fDirection
    real*8 :: fR
    integer:: fType
    real*8 :: fTAU, fAMP
    select case(fDirection)
       case(-1)
          call setfeed(feedNo, x+1,y,z, x+1-fLength,y,z, fR, fType, fTAU, fAMP)
       case(1)
          call setfeed(feedNo, x+1-fLength,y,z, x+1,y,z, fR, fType, fTAU, fAMP)
       case(-2)
          call setfeed(feedNo, x,y+1,z, x,y+1-fLength,z, fR, fType, fTAU, fAMP)
       case(2)
          call setfeed(feedNo, x,y+1-fLength,z, x,y+1,z, fR, fType, fTAU, fAMP)
       case(-3)
          call setfeed(feedNo, x,y,z+1, x,y,z+1-fLength, fR, fType, fTAU, fAMP)
       case(3)
          call setfeed(feedNo, x,y,z+1-fLength, x,y,z+1, fR, fType, fTAU, fAMP)
       case default
          stop 'Error. (ASoCsetfeed)'
    end select
  end subroutine ASoCsetfeed
  subroutine ARsetline(materialID, x1, y1, z1, x2, y2, z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i
    
    if ((x2 > x1) .and. (y1 == y2) .and. (z1 == z2)) then
       if((nzS(id) <= z1) .and. (z1 <= nzE(id))) then
          IDEx(x1:x2-1,y1,z1) = materialID
       endif
    else if ((x1 == x2) .and. (y2 > y1) .and. (z1 == z2)) then
       if((nzS(id) <= z1) .and. (z1 <= nzE(id))) then
          IDEy(x1,y1:y2-1,z1) = materialID
       endif
    else if ((x1 == x2) .and. (y1 == y2) .and. (z2 > z1)) then
       do i=z1, z2-1
          if((nzS(id) <= i) .and. (i <= nzE(id))) then
             IDEz(x1,y1,i) = materialID
          endif
       enddo
    else
       stop 'Error. (setline)'
    endif
  end subroutine ARsetline
  subroutine ASoCsetline(materialID, x1, y1, z1, x2, y2, z2, axis)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: axis
    select case(axis)
    case(1)
       call setline(materialID, x1, y1, z1, x2+1, y2, z2)
    case(2)
       call setline(materialID, x1, y1, z1, x2, y2+1, z2)
    case(3)
       call setline(materialID, x1, y1, z1, x2, y2, z2+1)
    case default
       stop 'Error. (ASoCsetline)'
    endselect
  end subroutine ASoCsetline
  subroutine ARsetplane(materialID, x1, y1, z1, x2, y2, z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i
    if ((x2 > x1) .and. (y2 > y1) .and. (z1 == z2)) then
       do i = x1, x2
          call setline(materialID, i, y1, z1, i, y2, z2)
       enddo
       do i = y1, y2
          call setline(materialID, x1, i, z1, x2, i, z2)
       enddo
    else if ((x1 == x2) .and. (y2 > y1) .and. (z2 > z1)) then
       do i = y1, y2
          call setline(materialID, x1, i, z1, x2, i, z2)
       enddo
       do i = z1, z2
          call setline(materialID, x1, y1, i, x2, y2, i)
       enddo
    else if ((x2 > x1) .and. (y1 == y2) .and. (z2 > z1)) then
       do i = z1, z2
          call setline(materialID, x1, y1, i, x2, y2, i)
       enddo
       do i = x1, x2
          call setline(materialID, i, y1, z1, i, y2, z2)
       enddo
    else
       stop 'Error. (setplane)'
    endif
  end subroutine ARsetplane
  subroutine ASoCsetplane(materialID, x1,y1,z1, x2,y2,z2, planeFlag)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: planeFlag
    select case(planeFlag)
    case(1)
       call setplane(materialID, x1, y1, z1, x2, y2+1, z2+1)
    case(2)
       call setplane(materialID, x1, y1, z1, x2+1, y2, z2+1)
    case(3)
       call setplane(materialID, x1, y1, z1, x2+1, y2+1, z2)
    case default
       stop 'Error. (ASoCsetplane)'
    endselect
  end subroutine ASoCsetplane
  subroutine setplane2(materialID, x1, y1, z1, x2, y2, z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i
    
    if ((x2 > x1) .and. (y2 > y1) .and. (z1 == z2)) then
       do i = x1+1, x2-1
          call setline(materialID, i, y1, z1, i, y2, z2)
       enddo
       do i = y1+1, y2-1
          call setline(materialID, x1, i, z1, x2, i, z2)
       enddo
    else if ((x1 == x2) .and. (y2 > y1) .and. (z2 > z1)) then
       do i = y1+1, y2-1
          call setline(materialID, x1, i, z1, x2, i, z2)
       enddo
       do i = z1+1, z2-1
          call setline(materialID, x1, y1, i, x2, y2, i)
       enddo
    else if ((x2 > x1) .and. (y1 == y2) .and. (z2 > z1)) then
       do i = z1+1, z2-1
          call setline(materialID, x1, y1, i, x2, y2, i)
       enddo
       do i = x1+1, x2-1
          call setline(materialID, i, y1, z1, i, y2, z2)
       enddo
    else
       stop 'Error. (setplane2)'
    endif
  end subroutine setplane2
  subroutine setbox(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    
    if(ModelingMode == 0) then
       call ARsetbox(materialID, x1,y1,z1, x2,y2,z2)
    else if(ModelingMode == 1) then
       call ASoCsetbox(materialID, x1,y1,z1, x2,y2,z2)
    end if
  end subroutine setbox
  subroutine ARsetbox(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i
    if ((x2 > x1) .and. (y2 > y1) .and. (z2 > z1)) then
       do i = x1, x2
          call setplane(materialID, i,y1,z1, i,y2,z2)
       enddo
       do i = y1, y2
          call setplane(materialID, x1,i,z1, x2,i,z2)
       enddo
       do i = z1, z2
          call setplane(materialID, x1,y1,i, x2,y2,i)
       enddo
    else
       stop 'Error. (setbox)'
    endif
  end subroutine ARsetbox
  subroutine ASoCsetbox(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    if ((x2 >= x1) .and. (y2 >= y1) .and. (z2 >= z1)) then
       call ARsetbox(materialID, x1,y1,z1 , x2+1,y2+1,z2+1)
    else
       stop 'Error. (ASoCsetbox)'
    endif
  end subroutine ASoCsetbox
  subroutine setbox2(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i
    if ((x2 > x1) .and. (y2 > y1) .and. (z2 > z1)) then
       do i = x1+1, x2-1
          call setplane2(materialID, i,y1,z1, i,y2,z2)
       enddo
       do i = y1+1, y2-1
          call setplane2(materialID, x1,i,z1, x2,i,z2)
       enddo
       do i = z1+1, z2-1
          call setplane2(materialID, x1,y1,i, x2,y2,i)
       enddo
    else
       stop 'Error. (setbox2)'
    endif
  end subroutine setbox2
  subroutine setoval(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    if ((x2 > x1) .and. (y2 > y1) .and. (z1 == z2)) then
       call setxyoval(materialID, x1,y1,z1 , x2,y2,z2)
    else if ((x1 == x2) .and. (y2 > y1) .and. (z2 > z1)) then
       call setyzoval(materialID, x1,y1,z1 , x2,y2,z2)
    else if ((x2 > x1) .and. (y1 == y2) .and. (z2 > z1)) then
       call setzxoval(materialID, x1,y1,z1 , x2,y2,z2)
    else
       stop 'Error. (setoval)'
    endif
  end subroutine setoval
  subroutine setxyoval(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j
    real*8 :: a, b, xc, yc
    real*8 :: y(x1:x2), x(y1:y2)
    a = dble(x2 - x1)*0.5d0*1.000001d0
    b = dble(y2 - y1)*0.5d0*1.000001d0
    xc = dble(x1 + x2)*0.5d0
    yc = dble(y1 + y2)*0.5d0
    do j=y1, y2
       x(j) = -a*sqrt(1.0d0 - (dble(j-yc)**2)/dble(b**2)) +xc
    enddo
    do i=x1, x2
       y(i) = -b*sqrt(1.0d0 - (dble(i-xc)**2)/dble(a**2)) +yc
    enddo
    do j=y1, int(yc - 0.01d0)
       do i=x1, int(xc - 0.01d0)
          if(x(j) < i)then
             ! OK
             call setplane(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
             exit
          else if((i < x(j)) .and. (x(j) < i+1)) then
             if((j < y(i)) .and. y(i) < j+1) then
           ! OK
                call setplane(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
                exit
             else
                ! [/]type
                if(((dble(i+1)-x(j))+(dble(i+1)-x(j+1))) > 1.0d0) then
                   call setplane(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
                   exit
           endif
             endif
          else
             if(y(i) < j+1) then
                ![/] type
                if(((dble(j+1)-y(i))+(dble(j+1)-y(i+1))) > 1.0d0) then
                   call setplane(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
                   exit
                endif
             endif
          endif
       enddo
    enddo
  end subroutine setxyoval
  subroutine setyzoval(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j
    real*8 :: a, b, yc, zc
    real*8 :: y(z1:z2), z(y1:y2)
    a = dble(y2 - y1)*0.5d0*1.000001d0
    b = dble(z2 - z1)*0.5d0*1.000001d0
    yc = dble(y1 + y2)*0.5d0
    zc = dble(z1 + z2)*0.5d0
    do j=z1, z2
       y(j) = -a*sqrt(1.0d0 - (dble(j-zc)**2)/dble(b**2)) +yc
    enddo
    do i=y1, y2
       z(i) = -b*sqrt(1.0d0 - (dble(i-yc)**2)/dble(a**2)) +zc
    enddo
    do j=z1, int(zc - 0.01d0)
       do i=y1, int(yc - 0.01d0)
          if(y(j) < i)then
             call setplane(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
             exit
          else if((i < y(j)) .and. (y(j) < i+1)) then
             if((j < z(i)) .and. z(i) < j+1) then
                call setplane(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
                exit
             else
                if(((dble(i+1)-y(j))+(dble(i+1)-y(j+1))) > 1.0d0) then
                   call setplane(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
                   exit
           endif
             endif
          else
             if(z(i) < j+1) then
                if(((dble(j+1)-z(i))+(dble(j+1)-z(i+1))) > 1.0d0) then
                   call setplane(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
                   exit
                endif
             endif
          endif
       enddo
    enddo
  end subroutine setyzoval
  subroutine setzxoval(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j
    real*8 :: a, b, zc, xc
    real*8 :: z(x1:x2), x(z1:z2)
    a = dble(z2 - z1)*0.5d0*1.000001d0
    b = dble(x2 - x1)*0.5d0*1.000001d0
    zc = dble(z1 + z2)*0.5d0
    xc = dble(x1 + x2)*0.5d0
    do j=x1, x2
       z(j) = -a*sqrt(1.0d0 - (dble(j-xc)**2)/dble(b**2)) +zc
    enddo
    do i=z1, z2
       x(i) = -b*sqrt(1.0d0 - (dble(i-zc)**2)/dble(a**2)) +xc
    enddo
    do j=x1, int(xc - 0.01d0)
       do i=z1, int(zc - 0.01d0)
          if(z(j) < i)then
             call setplane(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
             exit
          else if((i < z(j)) .and. (z(j) < i+1)) then
             if((j < x(i)) .and. x(i) < j+1) then
                call setplane(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
                exit
             else
                if(((dble(i+1)-z(j))+(dble(i+1)-z(j+1))) > 1.0d0) then
                   call setplane(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
                   exit
           endif
             endif
          else
             if(x(i) < j+1) then
                if(((dble(j+1)-x(i))+(dble(j+1)-x(i+1))) > 1.0d0) then
                   call setplane(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
                   exit
                endif
             endif
          endif
       enddo
    enddo
  end subroutine setzxoval
  
  subroutine setovalCP(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    if ((x2 > x1) .and. (y2 > y1) .and. (z1 == z2)) then
       call setxyovalCP(materialID, x1,y1,z1 , x2,y2,z2)
    else if ((x1 == x2) .and. (y2 > y1) .and. (z2 > z1)) then
       call setyzovalCP(materialID, x1,y1,z1 , x2,y2,z2)
    else if ((x2 > x1) .and. (y1 == y2) .and. (z2 > z1)) then
       call setzxovalCP(materialID, x1,y1,z1 , x2,y2,z2)
    else
       stop 'Error. (setovalCP)'
    endif
  end subroutine setovalCP
  subroutine setxyovalCP(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j, i2, j2
    real*8 :: a, b, xc, yc
    real*8 :: y(x1:x2), x(y1:y2)
    a = dble(x2 - x1)*0.5d0*1.000001d0
    b = dble(y2 - y1)*0.5d0*1.000001d0
    xc = dble(x1 + x2)*0.5d0
    yc = dble(y1 + y2)*0.5d0
    do j=y1, y2
       x(j) = -a*sqrt(1.0d0 - (dble(j-yc)**2)/dble(b**2)) +xc
    enddo
    do i=x1, x2
       y(i) = -b*sqrt(1.0d0 - (dble(i-xc)**2)/dble(a**2)) +yc
    enddo
    do j=y1, int(yc - 0.01d0)
       j2 = nint(2.0d0*yc) - j - 1
       do i=x1, int(xc - 0.01d0)
          i2 = nint(2.0d0*xc) - i - 1
          if(x(j) < i)then
             ! OK
             call setplane(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
             exit
          else if((i < x(j)) .and. (x(j) < i+1)) then
             if((j < y(i)) .and. y(i) < j+1) then
           !   ____
                ! $(B!?(B    |
                ! |     | pattern
                ! |____M|
                !
                call setCP(materialID, i,j,z1, &
                     & x(j)-dble(i), y(i)-dble(j), 1.0d0, &
                     & 0.0d0, 0.0d0, 1.0d0, &
                     & (x(j)-dble(i))*(y(i)-dble(j))*0.5d0, 3)
                call setCP(materialID, i,j2,z1, &
                     & 0.0d0, y(i)-dble(j), 1.0d0, &
                     & x(j)-dble(i), 0.0d0, 1.0d0, &
                     & (x(j)-dble(i))*(y(i)-dble(j))*0.5d0, 3)
                call setCP(materialID, i2,j,z1, &
                     & x(j)-dble(i), 0.0d0, 1.0d0, &
                     & 0.0d0, y(i)-dble(j), 1.0d0, &
                     & (x(j)-dble(i))*(y(i)-dble(j))*0.5d0, 3)
                call setCP(materialID, i2,j2,z1, &
                     & 0.0d0, 0.0d0, 1.0d0, &
                     & x(j)-dble(i), y(i)-dble(j), 1.0d0, &
                     & (x(j)-dble(i))*(y(i)-dble(j))*0.5d0, 3)
                cycle
             else
                !  _____
                ! |   / |
                ! |  /  | pattern
                ! |_/___|
                !
                call setCP(materialID, i,j,z1, &
                     & x(j)-dble(i), 1.0d0, 1.0d0, &
                     & x(j+1)-dble(i), 0.0d0, 1.0d0, &
                     & (x(j)-dble(i)+x(j+1)-dble(i))*0.5d0, 3)
                call setCP(materialID, i,j2,z1, &
                     & x(j+1)-dble(i), 1.0d0, 1.0d0, &
                     & x(j)-dble(i), 0.0d0, 1.0d0, &
                     & (x(j)-dble(i)+x(j+1)-dble(i))*0.5d0, 3)
                call setCP(materialID, i2,j,z1, &
                     & x(j)-dble(i), 0.0d0, 1.0d0, &
                     & x(j+1)-dble(i), 1.0d0, 1.0d0, &
                     & (x(j)-dble(i)+x(j+1)-dble(i))*0.5d0, 3)
                call setCP(materialID, i2,j2,z1, &
                     & x(j+1)-dble(i), 0.0d0, 1.0d0, &
                     & x(j)-dble(i), 1.0d0, 1.0d0, &
                     & (x(j)-dble(i)+x(j+1)-dble(i))*0.5d0, 3)
                cycle
             endif
          else
             if(y(i) < j+1) then
                !  ______
                ! |    __|
                ! |__$(B!?(B  | pattern
                ! |______|
                !
                call setCP(materialID, i,j,z1, &
                     & 1.0d0, y(i)-dble(j), 1.0d0, &
                     & 0.0d0, y(i+1)-dble(j), 1.0d0, &
                     & (y(i)-dble(j)+y(i+1)-dble(j))*0.5d0, 3)
                call setCP(materialID, i,j2,z1, &
                     & 0.0d0, y(i)-dble(j), 1.0d0, &
                     & 1.0d0, y(i+1)-dble(j), 1.0d0, &
                     & (y(i)-dble(j)+y(i+1)-dble(j))*0.5d0, 3)
                call setCP(materialID, i2,j,z1, &
                     & 1.0d0, y(i+1)-dble(j), 1.0d0, &
                     & 0.0d0, y(i)-dble(j), 1.0d0, &
                     & (y(i)-dble(j)+y(i+1)-dble(j))*0.5d0, 3)
                call setCP(materialID, i2,j2,z1, &
                     & 0.0d0, y(i+1)-dble(j), 1.0d0, &
                     & 1.0d0, y(i)-dble(j), 1.0d0, &
                     & (y(i)-dble(j)+y(i+1)-dble(j))*0.5d0, 3)
                cycle
             else if(y(i+1) < j+1) then
                !  ______
                ! |Air   |
                ! |      | pattern
                ! |____$(B!?(B
                ! 
                call setCP(materialID, i,j,z1, &
                     & 1.0d0, 1.0d0, 1.0d0, &
                     & x(j+1)-dble(i), y(i+1)-dble(j), 1.0d0, &
                     & 1.0d0-(dble(i+1)-x(j+1))*(dble(j+1)-y(i+1))*0.5d0, 3)
                call setCP(materialID, i,j2,z1, &
                     & x(j+1)-dble(i), 1.0d0, 1.0d0, &
                     & 1.0d0, y(i+1)-dble(j), 1.0d0, &
                     & 1.0d0-(dble(i+1)-x(j+1))*(dble(j+1)-y(i+1))*0.5d0, 3)
                call setCP(materialID, i2,j,z1, &
                     & 1.0d0, y(i+1)-dble(j), 1.0d0, &
                     & x(j+1)-dble(i), 1.0d0, 1.0d0, &
                     & 1.0d0-(dble(i+1)-x(j+1))*(dble(j+1)-y(i+1))*0.5d0, 3)
                call setCP(materialID, i2,j2,z1, &
                     & x(j+1)-dble(i), y(i+1)-dble(j), 1.0d0, &
                     & 1.0d0, 1.0d0, 1.0d0, &
                     & 1.0d0-(dble(i+1)-x(j+1))*(dble(j+1)-y(i+1))*0.5d0, 3)
                cycle
             else
                ! out of circle
                cycle
             endif
          endif
       enddo
    enddo
  end subroutine setxyovalCP
  subroutine setyzovalCP(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j, i2, j2
    real*8 :: a, b, yc, zc
    real*8 :: y(z1:z2), z(y1:y2)
    a = dble(y2 - y1)*0.5d0*1.000001d0
    b = dble(z2 - z1)*0.5d0*1.000001d0
    yc = dble(y1 + y2)*0.5d0
    zc = dble(z1 + z2)*0.5d0
    do j=z1, z2
       y(j) = -a*sqrt(1.0d0 - (dble(j-zc)**2)/dble(b**2)) +yc
    enddo
    do i=y1, y2
       z(i) = -b*sqrt(1.0d0 - (dble(i-yc)**2)/dble(a**2)) +zc
    enddo
    do j=z1, int(zc - 0.01d0)
       j2 = nint(2.0d0*zc) - j - 1
       do i=y1, int(yc - 0.01d0)
          i2 = nint(2.0d0*yc) - i - 1
          if(y(j) < i)then
             call setplane(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
             exit
          else if((i < y(j)) .and. (y(j) < i+1)) then
             if((j < z(i)) .and. z(i) < j+1) then
           !   ____
                ! $(B!?(B    |
                ! |     | pattern
                ! |____M|
                !
                call setCP(materialID, x1,i,j, &
                     & 1.0d0, y(j)-dble(i), z(i)-dble(j), &
                     & 1.0d0, 0.0d0, 0.0d0, &
                     & (y(j)-dble(i))*(z(i)-dble(j))*0.5d0, 1)
                call setCP(materialID, x1,i,j2, &
                     & 1.0d0, 0.0d0, z(i)-dble(j), &
                     & 1.0d0, y(j)-dble(i), 0.0d0, &
                     & (y(j)-dble(i))*(z(i)-dble(j))*0.5d0, 1)
                call setCP(materialID, x1,i2,j, &
                     & 1.0d0, y(j)-dble(i), 0.0d0, &
                     & 1.0d0, 0.0d0, z(i)-dble(j), &
                     & (y(j)-dble(i))*(z(i)-dble(j))*0.5d0, 1)
                call setCP(materialID, x1,i2,j2, &
                     & 1.0d0, 0.0d0, 0.0d0, &
                     & 1.0d0, y(j)-dble(i), z(i)-dble(j), &
                     & (y(j)-dble(i))*(z(i)-dble(j))*0.5d0, 1)
                cycle
             else
                !  _____
                ! |   / |
                ! |  /  | pattern
                ! |_/___|
                !
                call setCP(materialID, x1,i,j, &
                     & 1.0d0, y(j)-dble(i), 1.0d0, &
                     & 1.0d0, y(j+1)-dble(i), 0.0d0, &
                     & (y(j)-dble(i)+y(j+1)-dble(i))*0.5d0, 1)
                call setCP(materialID, x1,i,j2, &
                     & 1.0d0, y(j+1)-dble(i), 1.0d0, &
                     & 1.0d0, y(j)-dble(i), 0.0d0, &
                     & (y(j)-dble(i)+y(j+1)-dble(i))*0.5d0, 1)
                call setCP(materialID, x1,i2,j, &
                     & 1.0d0, y(j)-dble(i), 0.0d0, &
                     & 1.0d0, y(j+1)-dble(i), 1.0d0, &
                     & (y(j)-dble(i)+y(j+1)-dble(i))*0.5d0, 1)
                call setCP(materialID, x1,i2,j2, &
                     & 1.0d0, y(j+1)-dble(i), 0.0d0, &
                     & 1.0d0, y(j)-dble(i), 1.0d0, &
                     & (y(j)-dble(i)+y(j+1)-dble(i))*0.5d0, 1)
                cycle
             endif
          else
             if(z(i) < j+1) then
                !  ______
                ! |    __|
                ! |__$(B!?(B  | pattern
                ! |______|
                !
                call setCP(materialID, x1,i,j, &
                     & 1.0d0, 1.0d0, z(i)-dble(j), &
                     & 1.0d0, 0.0d0, z(i+1)-dble(j), &
                     & (z(i)-dble(j)+z(i+1)-dble(j))*0.5d0, 1)
                call setCP(materialID, x1,i,j2, &
                     & 1.0d0, 0.0d0, z(i)-dble(j), &
                     & 1.0d0, 1.0d0, z(i+1)-dble(j), &
                     & (z(i)-dble(j)+z(i+1)-dble(j))*0.5d0, 1)
                call setCP(materialID, x1,i2,j, &
                     & 1.0d0, 1.0d0, z(i+1)-dble(j), &
                     & 1.0d0, 0.0d0, z(i)-dble(j), &
                     & (z(i)-dble(j)+z(i+1)-dble(j))*0.5d0, 1)
                call setCP(materialID, x1,i2,j2, &
                     & 1.0d0, 0.0d0, z(i+1)-dble(j), &
                     & 1.0d0, 1.0d0, z(i)-dble(j), &
                     & (z(i)-dble(j)+z(i+1)-dble(j))*0.5d0, 1)
                cycle
             else if(z(i+1) < j+1) then
                !  ______
                ! |Air   |
                ! |      | pattern
                ! |____$(B!?(B
                ! 
                call setCP(materialID, x1,i,j, &
                     & 1.0d0, 1.0d0, 1.0d0, &
                     & 1.0d0, y(j+1)-dble(i), z(i+1)-dble(j), &
                     & 1.0d0-(dble(i+1)-y(j+1))*(dble(j+1)-z(i+1))*0.5d0, 1)
                call setCP(materialID, x1,i,j2, &
                     & 1.0d0, y(j+1)-dble(i), 1.0d0, &
                     & 1.0d0, 1.0d0, z(i+1)-dble(j), &
                     & 1.0d0-(dble(i+1)-y(j+1))*(dble(j+1)-z(i+1))*0.5d0, 1)
                call setCP(materialID, x1,i2,j, &
                     & 1.0d0, 1.0d0, z(i+1)-dble(j), &
                     & 1.0d0, y(j+1)-dble(i), 1.0d0, &
                     & 1.0d0-(dble(i+1)-y(j+1))*(dble(j+1)-z(i+1))*0.5d0, 1)
                call setCP(materialID, x1,i2,j2, &
                     & 1.0d0, y(j+1)-dble(i), z(i+1)-dble(j), &
                     & 1.0d0, 1.0d0, 1.0d0, &
                     & 1.0d0-(dble(i+1)-y(j+1))*(dble(j+1)-z(i+1))*0.5d0, 1)
                cycle
             else
                ! out of circle
                cycle
             end if
          endif
       enddo
    enddo
  end subroutine setyzovalCP
  subroutine setzxovalCP(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j, i2, j2
    real*8 :: a, b, zc, xc
    real*8 :: z(x1:x2), x(z1:z2)
    a = dble(z2 - z1)*0.5d0*1.000001d0
    b = dble(x2 - x1)*0.5d0*1.000001d0
    zc = dble(z1 + z2)*0.5d0
    xc = dble(x1 + x2)*0.5d0
    do j=x1, x2
       z(j) = -a*sqrt(1.0d0 - (dble(j-xc)**2)/dble(b**2)) +zc
    enddo
    do i=z1, z2
       x(i) = -b*sqrt(1.0d0 - (dble(i-zc)**2)/dble(a**2)) +xc
    enddo
    do j=x1, int(xc - 0.01d0)
       j2 = nint(2.0d0*xc) - j - 1
       do i=z1, int(zc - 0.01d0)
          i2 = nint(2.0d0*zc) - i - 1
          if(z(j) < i)then
             call setplane(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
             exit
          else if((i < z(j)) .and. (z(j) < i+1)) then
             if((j < x(i)) .and. x(i) < j+1) then
           !   ____
                ! $(B!?(B    |
                ! |     | pattern
                ! |____M|
                !
                call setCP(materialID, j,y1,i, &
                     & x(i)-dble(j), 1.0d0, z(j)-dble(i), &
                     & 0.0d0, 1.0d0, 0.0d0, &
                     & (z(j)-dble(i))*(x(i)-dble(j))*0.5d0, 2)
                call setCP(materialID, j2,y1,i, &
                     & x(i)-dble(j), 1.0d0, 0.0d0, &
                     & 0.0d0, 1.0d0, z(j)-dble(i), &
                     & (z(j)-dble(i))*(x(i)-dble(j))*0.5d0, 2)
                call setCP(materialID, j,y1,i2, &
                     & 0.0d0, 1.0d0, z(j)-dble(i), &
                     & x(i)-dble(j), 1.0d0, 0.0d0, &
                     & (z(j)-dble(i))*(x(i)-dble(j))*0.5d0, 2)
                call setCP(materialID, j2,y1,i2, &
                     & 0.0d0, 1.0d0, 0.0d0, &
                     & x(i)-dble(j), 1.0d0, z(j)-dble(i), &
                     & (z(j)-dble(i))*(x(i)-dble(j))*0.5d0, 2)
                cycle
             else
                !  _____
                ! |   / |
                ! |  /  | pattern
                ! |_/___|
                !
                call setCP(materialID, j,y1,i, &
                     & 1.0d0, 1.0d0, z(j)-dble(i), &
                     & 0.0d0, 1.0d0, z(j+1)-dble(i), &
                     & (z(j)-dble(i)+z(j+1)-dble(i))*0.5d0, 2)
                call setCP(materialID, j2,y1,i, &
                     & 1.0d0, 1.0d0, z(j+1)-dble(i), &
                     & 0.0d0, 1.0d0, z(j)-dble(i), &
                     & (z(j)-dble(i)+z(j+1)-dble(i))*0.5d0, 2)
                call setCP(materialID, j,y1,i2, &
                     & 0.0d0, 1.0d0, z(j)-dble(i), &
                     & 1.0d0, 1.0d0, z(j+1)-dble(i), &
                     & (z(j)-dble(i)+z(j+1)-dble(i))*0.5d0, 2)
                call setCP(materialID, j2,y1,i2, &
                     & 0.0d0, 1.0d0, z(j+1)-dble(i), &
                     & 1.0d0, 1.0d0, z(j)-dble(i), &
                     & (z(j)-dble(i)+z(j+1)-dble(i))*0.5d0, 2)
                cycle
             endif
          else
             if(x(i) < j+1) then
                !  ______
                ! |    __|
                ! |__$(B!?(B  | pattern
                ! |______|
                !
                call setCP(materialID, j,y1,i, &
                     & x(i)-dble(j),1.0d0, 1.0d0, &
                     & x(i+1)-dble(j), 1.0d0, 0.0d0, &
                     & (x(i)-dble(j)+x(i+1)-dble(j))*0.5d0, 2)
                call setCP(materialID, j2,y1,i, &
                     & x(i)-dble(j),1.0d0, 0.0d0, &
                     & x(i+1)-dble(j), 1.0d0, 1.0d0, &
                     & (x(i)-dble(j)+x(i+1)-dble(j))*0.5d0, 2)
                call setCP(materialID, j,y1,i2, &
                     & x(i+1)-dble(j),1.0d0, 1.0d0, &
                     & x(i)-dble(j), 1.0d0, 0.0d0, &
                     & (x(i)-dble(j)+x(i+1)-dble(j))*0.5d0, 2)
                call setCP(materialID, j2,y1,i2, &
                     & x(i+1)-dble(j),1.0d0, 0.0d0, &
                     & x(i)-dble(j), 1.0d0, 1.0d0, &
                     & (x(i)-dble(j)+x(i+1)-dble(j))*0.5d0, 2)
                cycle
             else if(x(i+1) < j+1) then
                !  ______
                ! |Air   |
                ! |      | pattern
                ! |____$(B!?(B
                ! 
                call setCP(materialID, j,y1,i, &
                     & 1.0d0, 1.0d0, 1.0d0, &
                     & x(i+1)-dble(j),1.0d0, z(j+1)-dble(i), &
                     & 1.0d0-(dble(i+1)-z(j+1))*(dble(j+1)-x(i+1))*0.5d0, 2)
                call setCP(materialID, j2,y1,i, &
                     & 1.0d0, 1.0d0, z(j+1)-dble(i), &
                     & x(i+1)-dble(j),1.0d0, 1.0d0, &
                     & 1.0d0-(dble(i+1)-z(j+1))*(dble(j+1)-x(i+1))*0.5d0, 2)
                call setCP(materialID, j,y1,i2, &
                     & x(i+1)-dble(j), 1.0d0, 1.0d0, &
                     & 1.0d0, 1.0d0, z(j+1)-dble(i), &
                     & 1.0d0-(dble(i+1)-z(j+1))*(dble(j+1)-x(i+1))*0.5d0, 2)
                call setCP(materialID, j2,y1,i2, &
                     & x(i+1)-dble(j), 1.0d0, z(j+1)-dble(i), &
                     & 1.0d0,1.0d0, 1.0d0, &
                     & 1.0d0-(dble(i+1)-z(j+1))*(dble(j+1)-x(i+1))*0.5d0, 2)
                cycle
             else
                ! out of circle
                cycle
             endif
          endif
       enddo
    enddo
  end subroutine setzxovalCP
  subroutine setoval2(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    if ((x2 > x1) .and. (y2 > y1) .and. (z1 == z2)) then
       call setxyoval2(materialID, x1,y1,z1 , x2,y2,z2)
    else if ((x1 == x2) .and. (y2 > y1) .and. (z2 > z1)) then
       call setyzoval2(materialID, x1,y1,z1 , x2,y2,z2)
    else if ((x2 > x1) .and. (y1 == y2) .and. (z2 > z1)) then
       call setzxoval2(materialID, x1,y1,z1 , x2,y2,z2)
    else
       stop 'Error. (setoval2)'
    endif
  end subroutine setoval2
  subroutine setxyoval2(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j
    real*8 :: a, b, xc, yc
    real*8 :: y(x1:x2), x(y1:y2)
    a = dble(x2 - x1)*0.5d0*1.000001d0
    b = dble(y2 - y1)*0.5d0*1.000001d0
    xc = dble(x1 + x2)*0.5d0
    yc = dble(y1 + y2)*0.5d0
    do j=y1, y2
       x(j) = -a*sqrt(1.0d0 - (dble(j-yc)**2)/dble(b**2)) +xc
    enddo
    do i=x1, x2
       y(i) = -b*sqrt(1.0d0 - (dble(i-xc)**2)/dble(a**2)) +yc
    enddo
    do j=y1, int(yc - 0.01d0)
       do i=x1, int(xc - 0.01d0)
          if(x(j) < i)then
             ! OK
             call setplane2(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
             exit
          else if((i < x(j)) .and. (x(j) < i+1)) then
             if((j < y(i)) .and. y(i) < j+1) then
           ! OK
                call setplane2(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
                exit
             else
                ! [/]type
                if(((dble(i+1)-x(j))+(dble(i+1)-x(j+1))) > 1.0d0) then
                   call setplane2(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
                   exit
           endif
             endif
          else
             if(y(i) < j+1) then
                ![/] type
                if(((dble(j+1)-y(i))+(dble(j+1)-y(i+1))) > 1.0d0) then
                   call setplane2(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
                   exit
                endif
             endif
          endif
       enddo
    enddo
  end subroutine setxyoval2
  subroutine setyzoval2(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j
    real*8 :: a, b, yc, zc
    real*8 :: y(z1:z2), z(y1:y2)
    a = dble(y2 - y1)*0.5d0*1.000001d0
    b = dble(z2 - z1)*0.5d0*1.000001d0
    yc = dble(y1 + y2)*0.5d0
    zc = dble(z1 + z2)*0.5d0
    do j=z1, z2
       y(j) = -a*sqrt(1.0d0 - (dble(j-zc)**2)/dble(b**2)) +yc
    enddo
    do i=y1, y2
       z(i) = -b*sqrt(1.0d0 - (dble(i-yc)**2)/dble(a**2)) +zc
    enddo
    do j=z1, int(zc - 0.01d0)
       do i=y1, int(yc - 0.01d0)
          if(y(j) < i)then
             call setplane2(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
             exit
          else if((i < y(j)) .and. (y(j) < i+1)) then
             if((j < z(i)) .and. z(i) < j+1) then
                call setplane2(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
                exit
             else
                if(((dble(i+1)-y(j))+(dble(i+1)-y(j+1))) > 1.0d0) then
                   call setplane2(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
                   exit
           endif
             endif
          else
             if(z(i) < j+1) then
                if(((dble(j+1)-z(i))+(dble(j+1)-z(i+1))) > 1.0d0) then
                   call setplane2(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
                   exit
                endif
             endif
          endif
       enddo
    enddo
  end subroutine setyzoval2
  subroutine setzxoval2(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j
    real*8 :: a, b, zc, xc
    real*8 :: z(x1:x2), x(z1:z2)
    a = dble(z2 - z1)*0.5d0*1.000001d0
    b = dble(x2 - x1)*0.5d0*1.000001d0
    zc = dble(z1 + z2)*0.5d0
    xc = dble(x1 + x2)*0.5d0
    do j=x1, x2
       z(j) = -a*sqrt(1.0d0 - (dble(j-xc)**2)/dble(b**2)) +zc
    enddo
    do i=z1, z2
       x(i) = -b*sqrt(1.0d0 - (dble(i-zc)**2)/dble(a**2)) +xc
    enddo
    do j=x1, int(xc - 0.01d0)
       do i=z1, int(zc - 0.01d0)
          if(z(j) < i)then
             call setplane2(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
             exit
          else if((i < z(j)) .and. (z(j) < i+1)) then
             if((j < x(i)) .and. x(i) < j+1) then
                call setplane2(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
                exit
             else
                if(((dble(i+1)-z(j))+(dble(i+1)-z(j+1))) > 1.0d0) then
                   call setplane2(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
                   exit
           endif
             endif
          else
             if(x(i) < j+1) then
                if(((dble(j+1)-x(i))+(dble(j+1)-x(i+1))) > 1.0d0) then
                   call setplane2(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
                   exit
                endif
             endif
          endif
       enddo
    enddo
  end subroutine setzxoval2
  
  subroutine setXY_Zcylinder(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j
    real*8 :: a, b, xc, yc
    real*8 :: y(x1:x2), x(y1:y2)
    a = dble(x2 - x1)*0.5d0*1.000001d0
    b = dble(y2 - y1)*0.5d0*1.000001d0
    xc = dble(x1 + x2)*0.5d0
    yc = dble(y1 + y2)*0.5d0
    do j=y1, y2
       x(j) = -a*sqrt(1.0d0 - (dble(j-yc)**2)/dble(b**2)) +xc
    enddo
    do i=x1, x2
       y(i) = -b*sqrt(1.0d0 - (dble(i-xc)**2)/dble(a**2)) +yc
    enddo
    do j=y1, int(yc - 0.01d0)
       do i=x1, int(xc - 0.01d0)
          if(x(j) < i)then
             ! OK
             call setbox(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
             exit
          else if((i < x(j)) .and. (x(j) < i+1)) then
             if((j < y(i)) .and. y(i) < j+1) then
           ! OK
                call setbox(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
                exit
             else
                ! [/]type
                if(((dble(i+1)-x(j))+(dble(i+1)-x(j+1))) > 1.0d0) then
                   call setbox(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
                   exit
           endif
             endif
          else
             if(y(i) < j+1) then
                ![/] type
                if(((dble(j+1)-y(i))+(dble(j+1)-y(i+1))) > 1.0d0) then
                   call setbox(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
                   exit
                endif
             endif
          endif
       enddo
    enddo
  end subroutine setXY_Zcylinder
  subroutine setYZ_Xcylinder(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j
    real*8 :: a, b, yc, zc
    real*8 :: y(z1:z2), z(y1:y2)
    a = dble(y2 - y1)*0.5d0*1.000001d0
    b = dble(z2 - z1)*0.5d0*1.000001d0
    yc = dble(y1 + y2)*0.5d0
    zc = dble(z1 + z2)*0.5d0
    do j=z1, z2
       y(j) = -a*sqrt(1.0d0 - (dble(j-zc)**2)/dble(b**2)) +yc
    enddo
    do i=y1, y2
       z(i) = -b*sqrt(1.0d0 - (dble(i-yc)**2)/dble(a**2)) +zc
    enddo
    do j=z1, int(zc - 0.01d0)
       do i=y1, int(yc - 0.01d0)
          if(y(j) < i)then
             call setbox(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
             exit
          else if((i < y(j)) .and. (y(j) < i+1)) then
             if((j < z(i)) .and. z(i) < j+1) then
                call setbox(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
                exit
             else
                if(((dble(i+1)-y(j))+(dble(i+1)-y(j+1))) > 1.0d0) then
                   call setbox(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
                   exit
           endif
             endif
          else
             if(z(i) < j+1) then
                if(((dble(j+1)-z(i))+(dble(j+1)-z(i+1))) > 1.0d0) then
                   call setbox(materialID, x1,i,j , x2,y2-i+y1,z2-j+z1)
                   exit
                endif
             endif
          endif
       enddo
    enddo
  end subroutine setYZ_Xcylinder
  subroutine setZX_Ycylinder(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j
    real*8 :: a, b, zc, xc
    real*8 :: z(x1:x2), x(z1:z2)
    
    a = dble(z2 - z1)*0.5d0*1.000001d0
    b = dble(x2 - x1)*0.5d0*1.000001d0
    zc = dble(z1 + z2)*0.5d0
    xc = dble(x1 + x2)*0.5d0
    
    do j=x1, x2
       z(j) = -a*sqrt(1.0d0 - (dble(j-xc)**2)/dble(b**2)) +zc
    enddo
    do i=z1, z2
       x(i) = -b*sqrt(1.0d0 - (dble(i-zc)**2)/dble(a**2)) +xc
    enddo
    do j=x1, int(xc - 0.01d0)
       do i=z1, int(zc - 0.01d0)
          if(z(j) < i)then
             call setbox(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
             exit
          else if((i < z(j)) .and. (z(j) < i+1)) then
             if((j < x(i)) .and. x(i) < j+1) then
                call setbox(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
                exit
             else
                if(((dble(i+1)-z(j))+(dble(i+1)-z(j+1))) > 1.0d0) then
                   call setbox(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
                   exit
           endif
             endif
          else
             if(x(i) < j+1) then
                if(((dble(j+1)-x(i))+(dble(j+1)-x(i+1))) > 1.0d0) then
                   call setbox(materialID, j,y1,i , x2-j+x1,y2,z2-i+z1)
                   exit
                endif
             endif
          endif
       enddo
    enddo
  end subroutine setZX_Ycylinder
  subroutine setXY_Zcylinder2(materialID, x1,y1,z1 , x2,y2,z2)
    integer :: materialID
    integer :: x1, y1, z1
    integer :: x2, y2, z2
    integer :: i, j
    real*8 :: a, b, xc, yc
    real*8 :: y(x1:x2), x(y1:y2)
    a = dble(x2 - x1)*0.5d0*1.000001d0
    b = dble(y2 - y1)*0.5d0*1.000001d0
    xc = dble(x1 + x2)*0.5d0
    yc = dble(y1 + y2)*0.5d0
    do j=y1, y2
       x(j) = -a*sqrt(1.0d0 - (dble(j-yc)**2)/dble(b**2)) +xc
    enddo
    do i=x1, x2
       y(i) = -b*sqrt(1.0d0 - (dble(i-xc)**2)/dble(a**2)) +yc
    enddo
    do j=y1, int(yc - 0.01d0)
       do i=x1, int(xc - 0.01d0)
          if(x(j) < i)then
             ! OK
             call setbox2(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
             exit
          else if((i < x(j)) .and. (x(j) < i+1)) then
             if((j < y(i)) .and. y(i) < j+1) then
           ! OK
                call setbox2(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
                exit
             else
                ! [/]type
                if(((dble(i+1)-x(j))+(dble(i+1)-x(j+1))) > 1.0d0) then
                   call setbox2(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
                   exit
           endif
             endif
          else
             if(y(i) < j+1) then
                ![/] type
                if(((dble(j+1)-y(i))+(dble(j+1)-y(i+1))) > 1.0d0) then
                   call setbox2(materialID, i,j,z1 , x2-i+x1,y2-j+y1,z2)
                   exit
                endif
             endif
          endif
       enddo
    enddo
  end subroutine setXY_Zcylinder2
  subroutine setsphere(materialID, xc, yc, zc, r)
    integer :: materialID, xc,yc,zc, r
    integer :: i
    
    call setbox(materialID, xc-r,yc-1,zc-1, xc+r,yc+1,zc+1)
    call setbox(materialID, xc-1,yc-r,zc-1, xc+1,yc+r,zc+1)
    call setbox(materialID, xc-1,yc-1,zc-r, xc+1,yc+1,zc+r)
    
    do i=2, r-1
       call setXY_Zcylinder(materialID, &
            & xc-i,yc-i,zc-nint(sqrt(dble(r*r - i*i))), &
            & xc+i, yc+i, zc+nint(sqrt(dble(r*r - i*i))))
    end do
    do i=2, r-1
       call setYZ_Xcylinder(materialID, &
            & xc-nint(sqrt(dble(r*r - i*i))),yc-i,zc-i, &
            & xc+nint(sqrt(dble(r*r - i*i))), yc+i, zc+i)
    end do
    do i=2, r-1
       call setZX_Ycylinder(materialID, &
            & xc-i,yc-nint(sqrt(dble(r*r - i*i))),zc-i, &
            & xc+i, yc+nint(sqrt(dble(r*r - i*i))), zc+i)
    end do
  end subroutine setsphere
  
  subroutine SetMaterialAtFeedPoint
    integer :: i,x,y,z, nfeed2
    nfeed2 = 0
    do i=1, nfeed
       select case (feedAxis(i))
       case('x')
          if((nzS(id) <= feedz(i)) .and. (feedz(i) <= nzE(id))) then
             do x=feedx(i), feedx(i)+(feedLength(i)-1)*feedSign(i), feedSign(i)
                IDEx(x,feedy(i),feedz(i)) = -1
             end do
          end if
          XC_pat = XC_pat + dble(feedx(i)) &
               & + dble(feedSign(i)*(feedLength(i)-1))/2.0d0
          YC_pat = YC_pat + dble(feedy(i))
          ZC_pat = ZC_pat + dble(feedz(i))
          nfeed2 = nfeed2 + 1
       case('y')
          if((nzS(id) <= feedz(i)) .and. (feedz(i) <= nzE(id))) then
             do y=feedy(i), feedy(i)+(feedLength(i)-1)*feedSign(i), feedSign(i)
                IDEy(feedx(i),y,feedz(i)) = -1
             end do
          end if
          XC_pat = XC_pat + dble(feedx(i))
          YC_pat = YC_pat + dble(feedy(i)) &
               & + dble(feedSign(i)*(feedLength(i)-1))/2.0d0
          ZC_pat = ZC_pat + dble(feedz(i))
          nfeed2 = nfeed2 + 1
       case('z')
          do z=feedz(i), feedz(i)+(feedLength(i)-1)*feedSign(i), feedSign(i)
             if((nzS(id) <= z) .and. (z <= nzE(id))) then
                IDEz(feedx(i),feedy(i),z) = -1
             end if
          end do
          XC_pat = XC_pat + dble(feedx(i))
          YC_pat = YC_pat + dble(feedy(i))
          ZC_pat = ZC_pat + dble(feedz(i)) &
               & + dble(feedSign(i)*(feedLength(i)-1))/2.0d0
          nfeed2 = nfeed2 + 1
       end select
    enddo
    
    XC_pat = XC_pat / dble(nfeed2)
    YC_pat = YC_pat / dble(nfeed2)
    ZC_pat = ZC_pat / dble(nfeed2)
  end subroutine SetMaterialAtFeedPoint
  
  subroutine setCP_integer(materialID, x,y,z, lx1,ly1,lz1, lx2,ly2,lz2, S, flag)
    integer :: materialID
    integer :: x,y,z, flag
    integer :: lx1,ly1,lz1, lx2,ly2,lz2, S
    
    call setCP_real(materialID, x,y,z, &
         & dble(lx1)*0.01d0, dble(ly1)*0.01d0, dble(lz1)*0.01d0, &
         & dble(lx2)*0.01d0, dble(ly2)*0.01d0, dble(lz2)*0.01d0, &
         & dble(S)*0.01d0, flag)
  end subroutine setCP_integer
  subroutine setCP_real(materialID, x,y,z, lx1,ly1,lz1, lx2,ly2,lz2, S, flag)
    integer :: materialID
    integer :: x,y,z
    real*8 :: lx1,ly1,lz1, lx2,ly2,lz2
    real*8 :: S
    integer :: flag
    integer, dimension(:), allocatable :: tempCPX, tempCPY, tempCPZ
    integer, dimension(:), allocatable :: tempCPplaneFlag
    real*8, dimension(:), allocatable :: templlx1, templly1, templlz1
    real*8, dimension(:), allocatable :: templlx2, templly2, templlz2
    real*8, dimension(:), allocatable :: tempsq
    
    trush = dble(materialID)
    cpPoint = cpPoint + 1
    
    if(CPFlag == 0) then
       if(cpPoint == 1) then
          write(*,*) 'WARNING : CPFlag is equal to 0.'
       end if
       return
    end if
    
    if(cpPoint /= 1) then
       allocate(tempCPX(cpPoint-1))
       allocate(tempCPY(cpPoint-1))
       allocate(tempCPZ(cpPoint-1))
       allocate(tempCPplaneFlag(cpPoint-1));
       allocate(templlx1(cpPoint-1)); allocate(templlx2(cpPoint-1))
       allocate(templly1(cpPoint-1)); allocate(templly2(cpPoint-1))
       allocate(templlz1(cpPoint-1)); allocate(templlz2(cpPoint-1))
       allocate(tempsq(cpPoint-1))
       tempCPX = cpX; tempCPY = cpY; tempCPZ = cpZ; tempCPplaneFlag = cpPlaneFlag
       templlx1 = llx1; templly1 = lly1; templlz1 = llz1
       templlx2 = llx2; templly2 = lly2; templlz2 = llz2
       tempsq = sq
       deallocate(cpX); deallocate(cpY); deallocate(cpZ)
       deallocate(cpPlaneFlag)
       deallocate(llx1); deallocate(lly1); deallocate(llz1)
       deallocate(llx2); deallocate(lly2); deallocate(llz2)
       deallocate(sq)
    end if
    allocate(cpX(cpPoint)); allocate(cpY(cpPoint)); allocate(cpZ(cpPoint))
    allocate(cpPlaneFlag(cpPoint))
    allocate(llx1(cpPoint)); allocate(lly1(cpPoint)); allocate(llz1(cpPoint))
    allocate(llx2(cpPoint)); allocate(lly2(cpPoint)); allocate(llz2(cpPoint))
    allocate(sq(cpPoint))
    if(cpPoint /= 1) then
       cpX(1:cpPoint-1) = tempCPX
       cpY(1:cpPoint-1) = tempCPY
       cpZ(1:cpPoint-1) = tempCPZ
       cpPlaneFlag(1:cpPoint-1) = tempCPplaneFlag
       llx1(1:cpPoint-1) = templlx1; llx2(1:cpPoint-1) = templlx2
       lly1(1:cpPoint-1) = templly1; lly2(1:cpPoint-1) = templly2
       llz1(1:cpPoint-1) = templlz1; llz2(1:cpPoint-1) = templlz2
       sq(1:cpPoint-1) = tempsq
       deallocate(tempCPX); deallocate(tempCPY); deallocate(tempCPZ)
       deallocate(tempCPplaneFlag)
       deallocate(templlx1); deallocate(templly1); deallocate(templlz1)
       deallocate(templlx2); deallocate(templly2); deallocate(templlz2)
       deallocate(tempsq)
    end if
    
    cpX(cpPoint) = x; cpY(cpPoint) = y; cpZ(cpPoint) = z
    cpPlaneFlag(cpPoint) = flag
    llx1(cpPoint) = lx1; lly1(cpPoint) = ly1; llz1(cpPoint) = lz1
    llx2(cpPoint) = lx2; lly2(cpPoint) = ly2; llz2(cpPoint) = lz2
    sq(cpPoint) = S
  end subroutine setCP_real
  subroutine setMaterialAtCPcell
    integer :: i, count
    
    if((CPFlag /= 1) .or. (cpPoint == 0)) then
       return
    end if
    
    do i=1, cpPoint
       if(llx1(i) < DBL_EPSILON) then
          call setline(1, cpX(i),cpY(i),cpZ(i), cpX(i)+1,cpY(i),cpZ(i))
       end if
       if(llx2(i) < DBL_EPSILON) then
          select case (cpPlaneFlag(i))
          case (2)
             call setline(1, cpX(i),cpY(i),cpZ(i)+1, cpX(i)+1,cpY(i),cpZ(i)+1)
          case (3)
             call setline(1, cpX(i),cpY(i)+1,cpZ(i), cpX(i)+1,cpY(i)+1,cpZ(i))
          end select
       end if
       if(lly1(i) < DBL_EPSILON) then
          call setline(1, cpX(i),cpY(i),cpZ(i), cpX(i),cpY(i)+1,cpZ(i))
       end if
       if(lly2(i) < DBL_EPSILON) then
          select case (cpPlaneFlag(i))
          case (1)
             call setline(1, cpX(i),cpY(i),cpZ(i)+1, cpX(i),cpY(i)+1,cpZ(i)+1)
          case (3)
             call setline(1, cpX(i)+1,cpY(i),cpZ(i), cpX(i)+1,cpY(i)+1,cpZ(i))
          end select
       end if
       if(llz1(i) < DBL_EPSILON) then
          call setline(1, cpX(i),cpY(i),cpZ(i), cpX(i),cpY(i),cpZ(i)+1)
       end if
       if(llz2(i) < DBL_EPSILON) then
          select case (cpPlaneFlag(i))
          case (1)
             call setline(1, cpX(i),cpY(i)+1,cpZ(i), cpX(i),cpY(i)+1,cpZ(i)+1)
          case (2)
             call setline(1, cpX(i)+1,cpY(i),cpZ(i), cpX(i)+1,cpY(i),cpZ(i)+1)
          end select
       end if
       count = 0
       if(llx1(i) < DBL_EPSILON) then
          count = count + 1
       end if
       if(llx2(i) < DBL_EPSILON) then
          count = count + 1
       end if
       if(lly1(i) < DBL_EPSILON) then
          count = count + 1
       end if
       if(lly2(i) < DBL_EPSILON) then
          count = count + 1
       end if
       if(llz1(i) < DBL_EPSILON) then
          count = count + 1
       end if
       if(llz2(i) < DBL_EPSILON) then
          count = count + 1
       end if
       if(count >= 3) then
          select case(cpPlaneFlag(i))
          case (1)
             call setplane(1, cpX(i),cpY(i),cpZ(i), cpX(i),cpY(i)+1,cpZ(i)+1)
          case (2)
             call setplane(1, cpX(i),cpY(i),cpZ(i), cpX(i)+1,cpY(i),cpZ(i)+1)
          case (3)
             call setplane(1, cpX(i),cpY(i),cpZ(i), cpX(i)+1,cpY(i)+1,cpZ(i))
          end select
          cpPlaneFlag(i) = -1
       end if
       if(sq(i) < 0.25d0) then
          select case(cpPlaneFlag(i))
             case (1)
                call setplane(1, cpX(i),cpY(i),cpZ(i), cpX(i),cpY(i)+1,cpZ(i)+1)
             case (2)
                call setplane(1, cpX(i),cpY(i),cpZ(i), cpX(i)+1,cpY(i),cpZ(i)+1)
             case (3)
                call setplane(1, cpX(i),cpY(i),cpZ(i), cpX(i)+1,cpY(i)+1,cpZ(i))
          end select
          cpPlaneFlag(i) = -1
       end if
    end do
    call sortCPcell
    call mountCPcell
    call realizeCPparameter
  end subroutine setMaterialAtCPcell
  subroutine sortCPcell
    integer i, j, min_j, min_z, itemp
    real*8 :: rtemp
    
    if(cpPoint == 1) then
       if(cpPlaneFlag(1) == -1) then
          cpPoint = 0
       end if
       return
    end if
    ! Selection sort
    do i=1, cpPoint-1
       ! Set default value.
       min_j = i
       if(cpPlaneFlag(i) == -1) then
          min_z = (nz+1)*(ny+1)*(nx+1)
       else
          min_z = cpZ(i)*(nx+1)*(ny+1) + cpY(i)*(nx+1) + cpX(i)
       end if
       ! Find minimum value.
       do j=i+1, cpPoint
          if(cpPlaneFlag(j) /= -1) then
             if(cpZ(j)*(nx+1)*(ny+1) + cpY(j)*(nx+1) + cpX(j) < min_z) then
                min_j = j
                min_z = cpZ(j)*(nx+1)*(ny+1) + cpY(j)*(nx+1) + cpX(j)
             end if
          end if
       end do
       ! Swap.
       if(min_j /= i) then
          itemp = cpX(i); cpX(i) = cpX(min_j); cpX(min_j) = itemp
          itemp = cpY(i); cpY(i) = cpY(min_j); cpY(min_j) = itemp
          itemp = cpZ(i); cpZ(i) = cpZ(min_j); cpZ(min_j) = itemp
          itemp = cpPlaneFlag(i)
          cpPlaneFlag(i) = cpPlaneFlag(min_j)
          cpPlaneFlag(min_j) = itemp
          rtemp = llx1(i); llx1(i) = llx1(min_j); llx1(min_j) = rtemp
          rtemp = lly1(i); lly1(i) = lly1(min_j); lly1(min_j) = rtemp
          rtemp = llz1(i); llz1(i) = llz1(min_j); llz1(min_j) = rtemp
          rtemp = llx2(i); llx2(i) = llx2(min_j); llx2(min_j) = rtemp
          rtemp = lly2(i); lly2(i) = lly2(min_j); lly2(min_j) = rtemp
          rtemp = llz2(i); llz2(i) = llz2(min_j); llz2(min_j) = rtemp
          rtemp = sq(i); sq(i) = sq(min_j); sq(min_j) = rtemp
       end if
       ! Finish sorting.
       if(min_z == (nz+1)*(ny+1)*(nx+1)) then
          cpPoint = i - 1
          exit
       end if
    end do
    ! If Only one CP cell exists.
    if(cpPlaneFlag(cpPoint) == -1) then
       cpPoint = cpPoint - 1
    end if
  end subroutine sortCPcell
  subroutine mountCPcell
    integer :: z, i, next_i
    
    cpS = -1
    cpE = -1
    
    if(cpPoint == 0) then
       return
    end if
    
    next_i = 1
    do z=1, nz
       if(next_i == cpPoint+1) then
          cycle
       end if
       do i=next_i, cpPoint
          if(cpZ(i) > z) then
             exit
          else if(cpZ(i) == z) then
             if(cpS(z) == -1) then
                cpS(z) = i
             end if
             cpE(z) = i
             next_i = i+1
             cycle
          else
             stop 'Program Error. (mountCPcell)'
          end if
       end do
    end do
  end subroutine mountCPcell
  subroutine realizeCPparameter
    integer :: i
    
    if(cpPoint == 0) then
       return
    end if
    allocate(CHCP(cpPoint))
    llx1 = llx1*dx; lly1 = lly1*dy; llz1 = llz1*dz
    llx2 = llx2*dx; lly2 = lly2*dy; llz2 = llz2*dz
    do i=1, cpPoint
       select case (cpPlaneFlag(i))
          case (1)
             sq(i) = sq(i)*dy*dz
             CHCP(i) = dt/mu0/sq(i)
          case (2)
             sq(i) = sq(i)*dz*dx
             CHCP(i) = dt/mu0/sq(i)
          case (3)
             sq(i) = sq(i)*dx*dy
             CHCP(i) = dt/mu0/sq(i)
       end select
    end do
  end subroutine realizeCPparameter
end module setup
module fdtd
  use setup
  implicit none
contains
  subroutine initialize
    real*8 :: Sglmax
    real*8 :: SglA(1:LM), SglB(1:LM)
    integer :: cost(1:nz), totalcost, idealcost
    integer, dimension(:), allocatable :: localcost
    integer :: surplusL, mincost, mincostP
    integer :: idS, idE, pmlS, pmlE, pmlE1, pmlS2
    integer :: i, k, p
    dt = CFL/(c*(((1.0d0/dx)**2)+((1.0d0/dy)**2)+((1.0d0/dz)**2))**0.5d0)
    df = 1.0d0/dt/nt
    call MPI_init(error_code)
    call MPI_comm_size(MPI_COMM_WORLD, np, error_code)
    call MPI_comm_rank(MPI_COMM_WORLD, id, error_code)
    call MPI_type_contiguous(nx*ny, MPI_DOUBLE_PRECISION, MatrixXY, error_code)
    call MPI_type_commit(MatrixXY, error_code)
    zm = id - 1
    zp = id + 1
    if(id == 0) then
       zm = MPI_PROC_NULL
    endif
    if(id == np-1) then
       zp = MPI_PROC_NULL
    endif
    ! PML cost
    do k=1, nz
       cost(k) = 24*nx*ny
    enddo
    ! EH cost
    do k=LM+1, nz-LM
       cost(k) = cost(k) - 9*(nx-2*LM)*(ny-2*LM)
    enddo
    ! Pattern cost
    if((patternFlag /= 0) .or. (efficiencyFlag /= 0)) then
       cost(ZS_pat) = cost(ZS_pat) &
            & + 4*freqPoint_pat*(XE_pat-XS_pat+1)*(YE_pat-YS_pat+1)
       cost(ZE_pat+1) = cost(ZE_pat+1) &
            & + 4*freqPoint_pat*(XE_pat-XS_pat+1)*(YE_pat-YS_pat+1)
       cost(ZS_pat:ZE_pat) = cost(ZS_pat:ZE_pat) &
            & + 4*freqPoint_pat*(XE_pat-XS_pat+1)*2 &
            & + 4*freqPoint_pat*(YE_pat-YS_pat+1)*2
    end if
    ! PLRC cost
    if(PLRCFlag /= 0) then
       do k=ZS_RC, ZE_RC
          cost(k) = cost(k) + 4*(XE_RC-XS_RC+1)*(YE_RC-YS_RC+1)
       end do
    end if
    ! CP cost
    if((CPFlag /= 0) .and. (cpPoint > 0)) then
       do i=1, cpPoint
          cost(cpZ(i)) = cost(cpZ(i)) + 7
       end do
    end if
    ! Current distribution cost
    if(CDFlag /= 0) then
       if((ZS_CD == ZE_CD) .and. (XS_CD /= XE_CD) .and. (YS_CD /= YE_CD)) then
          cost(ZS_CD) = cost(ZS_CD) + 4*freqPoint_CD*(XE_CD-XS_CD)*(YE_CD-YS_CD)
       else
          do k=ZS_CD, ZE_CD
             cost(k) = cost(k) + 4*freqPoint_CD*(XE_CD-XS_CD) &
                  & + 4*freqPoint_CD*(YE_CD-YS_CD)
          end do
       end if
    end if
    ! Electric field distribution cost
    if(EDFlag /= 0) then
       if(ZS_ED == ZE_ED) then
          cost(ZS_ED) = cost(ZS_ED) + 4*freqPoint_ED*(XE_ED-XS_ED)*(YE_ED-YS_ED)
       else
          do k=ZS_ED, ZE_ED
             cost(k) = cost(k) + 4*freqPoint_ED*(XE_ED-XS_ED) &
                  & + 4*freqPoint_ED*(YE_ED-YS_ED)
          end do
       end if
    end if
    totalcost = sum(cost)
    idealcost = totalcost / np
    
    allocate(nzS(0:np))
    allocate(nzE(-1:np-1))
    allocate(nzL(0:np-1))
    allocate(localcost(0:np-1))
    nzE(-1) = 0
    nzS(np) = nz+1
    do p=0, int((np-1)/2)
       nzS(p) = nzE(p-1)+1
       nzE(p) = nzS(np-p)-1
       localcost(p) = 0
       do k=nzS(p), nzS(np-p)
          localcost(p) = localcost(p) + cost(k)
          if(localcost(p) > idealcost) then
             if((localcost(p)-idealcost) < (idealcost-localcost(p)+cost(k))) then
                nzE(p) = k
             else
                nzE(p) = k-1
                localcost(p) = localcost(p) - cost(k)
             endif
             exit
          endif
       enddo
       nzL(p) = nzE(p) - nzS(p) + 1
       nzS(np-p-1) = nzE(p)+1
       nzE(np-p-1) = nzS(np-p)-1
       localcost(np-p-1) = 0
       do k=nzE(np-p-1), nzE(p)+1, -1
          localcost(np-p-1) = localcost(np-p-1) + cost(k)
          if(localcost(np-p-1) > idealcost) then
             if((localcost(np-p-1)-idealcost) & 
                  & < (idealcost-localcost(np-p-1)+cost(k))) then
                nzS(np-p-1) = k
             else
                nzS(np-p-1) = k+1
                localcost(np-p-1) = localcost(np-p-1) - cost(k)
             endif
             exit
          endif
       enddo
       nzL(np-p-1) = nzE(np-p-1) - nzS(np-p-1) + 1
    enddo
    surplusL = nz
    do p=0, np-1
       surplusL = surplusL - nzL(p)
    enddo
    if(surplusL > 0) then
       do i=1, surplusL
          mincost = localcost(0)
          mincostP = 0
          do p=0, np-1
             if(localcost(p) < mincost) then
                mincost = localcost(p)
                mincostP = p
             endif
          enddo
          if(mincostP <= int(np/2)-1) then
             nzS(mincostP) = nzS(mincostP) - 1
             nzL(mincostP) = nzL(mincostP) + 1
             do p=mincostP, int(np/2)-1
                nzS(p) = nzS(p)+1
                nzE(p) = nzE(p)+1
                localcost(p) = 0
                do k=nzS(p), nzE(p)
                   localcost(p) = localcost(p) + cost(k)
                enddo
             enddo
          else
             nzE(mincostP) = nzE(mincostP) + 1
             nzL(mincostP) = nzL(mincostP) + 1
             do p=mincostP, int(np/2), -1
                nzE(p) = nzE(p) - 1
                nzS(p) = nzS(p) - 1
                localcost(p) = 0
                do k=nzS(p), nzE(p)
                   localcost(p) = localcost(p) + cost(k)
                enddo
             enddo
          endif
       enddo
    endif
    deallocate(localcost)
    allocate(Ex(1:nx, 1:ny, nzS(id):nzE(id)+1))
    allocate(Ey(1:nx, 1:ny, nzS(id):nzE(id)+1))
    allocate(Ez(1:nx, 1:ny, nzS(id):nzE(id)))
    allocate(Hx(1:nx, 1:ny, nzS(id)-1:nzE(id)))
    allocate(Hy(1:nx, 1:ny, nzS(id)-1:nzE(id)))
    allocate(Hz(1:nx, 1:ny, nzS(id):nzE(id)))
    Ex = 0.0d0; Ey = 0.0d0; Ez = 0.0d0
    Hx = 0.0d0; Hy = 0.0d0; Hz = 0.0d0
    if(LM < nzS(id)) then
       idS = nzS(id)
    else
       idS = LM+1
    endif
    if(nzE(id) < nz-LM+1) then
       idE = nzE(id)
    else
       idE = nz-LM
    endif
    if(idS <= idE) then
       allocate(IDEx(LM+1:nx-LM, LM+1:ny-LM, idS:idE))
       allocate(IDEy(LM+1:nx-LM, LM+1:ny-LM, idS:idE))
       allocate(IDEz(LM+1:nx-LM, LM+1:ny-LM, idS:idE))
       allocate(IDHx(LM+1:nx-LM, LM+1:ny-LM, idS:idE))
       allocate(IDHy(LM+1:nx-LM, LM+1:ny-LM, idS:idE))
       allocate(IDHz(LM+1:nx-LM, LM+1:ny-LM, idS:idE))
       IDEx = 0; IDEy = 0; IDEz = 0
       IDHx = 0; IDHy = 0; IDHz = 0
    endif
    if(nzS(id) <= LM) then
       pmlS = nzS(id)
    else if(nz-LM+1 <= nzS(id)) then
       pmlS = nzS(id)
    else
       pmlS = nz-LM+1
    endif
    if(nzE(id) >= nz-LM+1) then
       pmlE = nzE(id)
    else if(LM >= nzE(id)) then
       pmlE = nzE(id)
    else
       pmlE = LM
    endif
    if(pmlS <= LM) then
       if(pmlE < nz-LM) then
          pmlE1 = pmlE
       else
          pmlE1 = LM
       endif
       allocate(Exy1(1:nx, 1:ny, pmlS:pmlE1))
       allocate(Exz1(1:nx, 1:ny, pmlS:pmlE1))
       allocate(Eyz1(1:nx, 1:ny, pmlS:pmlE1))
       allocate(Eyx1(1:nx, 1:ny, pmlS:pmlE1))
       allocate(Ezx1(1:nx, 1:ny, pmlS:pmlE1))
       allocate(Ezy1(1:nx, 1:ny, pmlS:pmlE1))
       allocate(Hxy1(1:nx, 1:ny, pmlS:pmlE1))
       allocate(Hxz1(1:nx, 1:ny, pmlS:pmlE1))
       allocate(Hyz1(1:nx, 1:ny, pmlS:pmlE1))
       allocate(Hyx1(1:nx, 1:ny, pmlS:pmlE1))
       allocate(Hzx1(1:nx, 1:ny, pmlS:pmlE1))
       allocate(Hzy1(1:nx, 1:ny, pmlS:pmlE1))
       Exy1 = 0.0d0; Exz1 = 0.0d0; Hxy1 = 0.0d0; Hxz1 = 0.0d0
       Eyz1 = 0.0d0; Eyx1 = 0.0d0; Hyz1 = 0.0d0; Hyx1 = 0.0d0
       Ezx1 = 0.0d0; Ezy1 = 0.0d0; Hzx1 = 0.0d0; Hzy1 = 0.0d0
    endif
    if(pmlE >= nz-LM+1) then
       if(pmlS > LM) then
          pmlS2 = pmlS
       else
          pmlS2 = nz-LM+1
       endif
       allocate(Exy2(1:nx, 1:ny, pmlS2:pmlE))
       allocate(Exz2(1:nx, 1:ny, pmlS2:pmlE))
       allocate(Eyz2(1:nx, 1:ny, pmlS2:pmlE))
       allocate(Eyx2(1:nx, 1:ny, pmlS2:pmlE))
       allocate(Ezx2(1:nx, 1:ny, pmlS2:pmlE))
       allocate(Ezy2(1:nx, 1:ny, pmlS2:pmlE))
       allocate(Hxy2(1:nx, 1:ny, pmlS2:pmlE))
       allocate(Hxz2(1:nx, 1:ny, pmlS2:pmlE))
       allocate(Hyz2(1:nx, 1:ny, pmlS2:pmlE))
       allocate(Hyx2(1:nx, 1:ny, pmlS2:pmlE))
       allocate(Hzx2(1:nx, 1:ny, pmlS2:pmlE))
       allocate(Hzy2(1:nx, 1:ny, pmlS2:pmlE))
       Exy2 = 0.0d0; Exz2 = 0.0d0; Hxy2 = 0.0d0; Hxz2 = 0.0d0
       Eyz2 = 0.0d0; Eyx2 = 0.0d0; Hyz2 = 0.0d0; Hyx2 = 0.0d0
       Ezx2 = 0.0d0; Ezy2 = 0.0d0; Hzx2 = 0.0d0; Hzy2 = 0.0d0
    endif
    if(idE >= idS) then
       allocate(Exy3(1:nx, 1:LM, idS:idE)); allocate(Exz3(1:nx, 1:LM, idS:idE))
       allocate(Eyz3(1:nx, 1:LM, idS:idE)); allocate(Eyx3(1:nx, 1:LM, idS:idE))
       allocate(Ezx3(1:nx, 1:LM, idS:idE)); allocate(Ezy3(1:nx, 1:LM, idS:idE))
       allocate(Hxy3(1:nx, 1:LM, idS:idE)); allocate(Hxz3(1:nx, 1:LM, idS:idE))
       allocate(Hyz3(1:nx, 1:LM, idS:idE)); allocate(Hyx3(1:nx, 1:LM, idS:idE))
       allocate(Hzx3(1:nx, 1:LM, idS:idE)); allocate(Hzy3(1:nx, 1:LM, idS:idE))
       Exy3 = 0.0d0; Exz3 = 0.0d0; Hxy3 = 0.0d0; Hxz3 = 0.0d0
       Eyz3 = 0.0d0; Eyx3 = 0.0d0; Hyz3 = 0.0d0; Hyx3 = 0.0d0
       Ezx3 = 0.0d0; Ezy3 = 0.0d0; Hzx3 = 0.0d0; Hzy3 = 0.0d0
       allocate(Exy4(1:nx, ny-LM+1:ny, idS:idE))
       allocate(Exz4(1:nx, ny-LM+1:ny, idS:idE))
       allocate(Eyz4(1:nx, ny-LM+1:ny, idS:idE))
       allocate(Eyx4(1:nx, ny-LM+1:ny, idS:idE))
       allocate(Ezx4(1:nx, ny-LM+1:ny, idS:idE))
       allocate(Ezy4(1:nx, ny-LM+1:ny, idS:idE))
       allocate(Hxy4(1:nx, ny-LM+1:ny, idS:idE))
       allocate(Hxz4(1:nx, ny-LM+1:ny, idS:idE))
       allocate(Hyz4(1:nx, ny-LM+1:ny, idS:idE))
       allocate(Hyx4(1:nx, ny-LM+1:ny, idS:idE))
       allocate(Hzx4(1:nx, ny-LM+1:ny, idS:idE))
       allocate(Hzy4(1:nx, ny-LM+1:ny, idS:idE))
       Exy4 = 0.0d0; Exz4 = 0.0d0; Hxy4 = 0.0d0; Hxz4 = 0.0d0 
       Eyz4 = 0.0d0; Eyx4 = 0.0d0; Hyz4 = 0.0d0; Hyx4 = 0.0d0 
       Ezx4 = 0.0d0; Ezy4 = 0.0d0; Hzx4 = 0.0d0; Hzy4 = 0.0d0
       allocate(Exy5(1:LM, LM+1:ny-LM, idS:idE))
       allocate(Exz5(1:LM, LM+1:ny-LM, idS:idE))
       allocate(Eyz5(1:LM, LM+1:ny-LM, idS:idE))
       allocate(Eyx5(1:LM, LM+1:ny-LM, idS:idE))
       allocate(Ezx5(1:LM, LM+1:ny-LM, idS:idE))
       allocate(Ezy5(1:LM, LM+1:ny-LM, idS:idE))
       allocate(Hxy5(1:LM, LM+1:ny-LM, idS:idE))
       allocate(Hxz5(1:LM, LM+1:ny-LM, idS:idE))
       allocate(Hyz5(1:LM, LM+1:ny-LM, idS:idE))
       allocate(Hyx5(1:LM, LM+1:ny-LM, idS:idE))
       allocate(Hzx5(1:LM, LM+1:ny-LM, idS:idE))
       allocate(Hzy5(1:LM, LM+1:ny-LM, idS:idE))
       Exy5 = 0.0d0; Exz5 = 0.0d0; Hxy5 = 0.0d0; Hxz5 = 0.0d0
       Eyz5 = 0.0d0; Eyx5 = 0.0d0; Hyz5 = 0.0d0; Hyx5 = 0.0d0
       Ezx5 = 0.0d0; Ezy5 = 0.0d0; Hzx5 = 0.0d0; Hzy5 = 0.0d0
       allocate(Exy6(nx-LM+1:nx, LM+1:ny-LM, idS:idE))
       allocate(Exz6(nx-LM+1:nx, LM+1:ny-LM, idS:idE))
       allocate(Eyz6(nx-LM+1:nx, LM+1:ny-LM, idS:idE))
       allocate(Eyx6(nx-LM+1:nx, LM+1:ny-LM, idS:idE))
       allocate(Ezx6(nx-LM+1:nx, LM+1:ny-LM, idS:idE))
       allocate(Ezy6(nx-LM+1:nx, LM+1:ny-LM, idS:idE))
       allocate(Hxy6(nx-LM+1:nx, LM+1:ny-LM, idS:idE))
       allocate(Hxz6(nx-LM+1:nx, LM+1:ny-LM, idS:idE))
       allocate(Hyz6(nx-LM+1:nx, LM+1:ny-LM, idS:idE))
       allocate(Hyx6(nx-LM+1:nx, LM+1:ny-LM, idS:idE))
       allocate(Hzx6(nx-LM+1:nx, LM+1:ny-LM, idS:idE))
       allocate(Hzy6(nx-LM+1:nx, LM+1:ny-LM, idS:idE))
       Exy6 = 0.0d0; Exz6 = 0.0d0; Hxy6 = 0.0d0; Hxz6 = 0.0d0
       Eyz6 = 0.0d0; Eyx6 = 0.0d0; Hyz6 = 0.0d0; Hyx6 = 0.0d0
       Ezx6 = 0.0d0; Ezy6 = 0.0d0; Hzx6 = 0.0d0; Hzy6 = 0.0d0
    endif
    Sglmax = -dble(MM+1)*eps0*c/(2.0d0*dble(LM)*dx)*log(10.0d0**(RC0/20.0d0))
    do i=1, LM
       SglA(i) = Sglmax*(((dble(LM-i)+1.0d0)/dble(LM))**MM)
       SglB(i) = Sglmax*(((dble(LM-i)+0.5d0)/dble(LM))**MM)
    enddo
    do i=1, LM
       CpmlEdx(i) = (1.0d0-SglA(i)*dt/(2.0d0*eps0)) &
            & / (1.0d0+SglA(i)*dt/(2.0d0*eps0))
       CpmlEdxL(i) = (dt/eps0) &
            & / (1.0d0+SglA(i)*dt/(2.0d0*eps0))/dx
       CpmlHdx(i) = (1.0d0-SglB(i)*dt/(2.0d0*eps0)) &
            & / (1.0d0+SglB(i)*dt/(2.0d0*eps0))
       CpmlHdxL(i) = (dt/mu0) &
            & / (1.0d0+SglB(i)*dt/(2.0d0*eps0))/dx
       CpmlEdx(nx-i+1) = (1.0d0-SglB(i)*dt/(2.0d0*eps0)) &
            & / (1.0d0+SglB(i)*dt/(2.0d0*eps0))
       CpmlEdxL(nx-i+1) = (dt/eps0) &
            & / (1.0d0+SglB(i)*dt/(2.0d0*eps0))/dx
       CpmlHdx(nx-i+1) = (1.0d0-SglA(i)*dt/(2.0d0*eps0)) &
            & / (1.0d0+SglA(i)*dt/(2.0d0*eps0))
       CpmlHdxL(nx-i+1) = (dt/mu0) &
            & / (1.0d0+SglA(i)*dt/(2.0d0*eps0))/dx
    enddo
    do i=LM+1, nx-LM
       CpmlEdx(i) = 1.0d0
       CpmlEdxL(i) = dt/eps0/dx
       CpmlHdx(i) = 1.0d0
       CpmlHdxL(i) = dt/mu0/dx
    enddo
    Sglmax = -dble(MM+1)*eps0*c/(2.0d0*dble(LM)*dy)*log(10.0d0**(RC0/20.0d0))
    do i=1, LM
       SglA(i) = Sglmax*(((dble(LM-i)+1.0d0)/dble(LM))**MM)
       SglB(i) = Sglmax*(((dble(LM-i)+0.5d0)/dble(LM))**MM)
    enddo
    do i=1, LM
       CpmlEdy(i) = (1.0d0-SglA(i)*dt/(2.0d0*eps0)) &
            & / (1.0d0+SglA(i)*dt/(2.0d0*eps0))
       CpmlEdyL(i) = (dt/eps0) &
            & / (1.0d0+SglA(i)*dt/(2.0d0*eps0))/dy
       CpmlHdy(i) = (1.0d0-SglB(i)*dt/(2.0d0*eps0)) &
            & / (1.0d0+SglB(i)*dt/(2.0d0*eps0))
       CpmlHdyL(i) = (dt/mu0) &
            & / (1.0d0+SglB(i)*dt/(2.0d0*eps0))/dy
       CpmlEdy(ny-i+1) = (1.0d0-SglB(i)*dt/(2.0d0*eps0)) &
            & / (1.0d0+SglB(i)*dt/(2.0d0*eps0))
       CpmlEdyL(ny-i+1) = (dt/eps0) &
            & / (1.0d0+SglB(i)*dt/(2.0d0*eps0))/dy
       CpmlHdy(ny-i+1) = (1.0d0-SglA(i)*dt/(2.0d0*eps0)) &
            & / (1.0d0+SglA(i)*dt/(2.0d0*eps0))
       CpmlHdyL(ny-i+1) = (dt/mu0) &
            & / (1.0d0+SglA(i)*dt/(2.0d0*eps0))/dy
    enddo
    do i=LM+1, ny-LM
       CpmlEdy(i) = 1.0d0
       CpmlEdyL(i) = dt/eps0/dy
       CpmlHdy(i) = 1.0d0
       CpmlHdyL(i) = dt/mu0/dy
    enddo
    Sglmax = -dble(MM+1)*eps0*c/(2.0d0*dble(LM)*dz)*log(10.0d0**(RC0/20.0d0))
    do i=1, LM
       SglA(i) = Sglmax*(((dble(LM-i)+1.0d0)/dble(LM))**MM)
       SglB(i) = Sglmax*(((dble(LM-i)+0.5d0)/dble(LM))**MM)
    enddo
    do i=1, LM
       CpmlEdz(i) = (1.0d0-SglA(i)*dt/(2.0d0*eps0)) &
            & / (1.0d0+SglA(i)*dt/(2.0d0*eps0))
       CpmlEdzL(i) = (dt/eps0) &
            & / (1.0d0+SglA(i)*dt/(2.0d0*eps0))/dz
       CpmlHdz(i) = (1.0d0-SglB(i)*dt/(2.0d0*eps0)) &
            & / (1.0d0+SglB(i)*dt/(2.0d0*eps0))
       CpmlHdzL(i) = (dt/mu0) &
            & / (1.0d0+SglB(i)*dt/(2.0d0*eps0))/dz
       CpmlEdz(nz-i+1) = (1.0d0-SglB(i)*dt/(2.0d0*eps0)) &
            & / (1.0d0+SglB(i)*dt/(2.0d0*eps0))
       CpmlEdzL(nz-i+1) = (dt/eps0) &
            & / (1.0d0+SglB(i)*dt/(2.0d0*eps0))/dz
       CpmlHdz(nz-i+1) = (1.0d0-SglA(i)*dt/(2.0d0*eps0)) &
            & / (1.0d0+SglA(i)*dt/(2.0d0*eps0))
       CpmlHdzL(nz-i+1) = (dt/mu0) &
            & / (1.0d0+SglA(i)*dt/(2.0d0*eps0))/dz
    enddo
    do i=LM+1, nz-LM
       CpmlEdz(i) = 1.0d0
       CpmlEdzL(i) = dt/eps0/dz
       CpmlHdz(i) = 1.0d0
       CpmlHdzL(i) = dt/mu0/dz
    enddo
    ! Special material
    ! 0 : void
    ! 1 : PEC
    ! -1 : don't update E-field
    CEx = (1.0d0-sgm0*dt/2.0d0/eps0)/(1.0d0+sgm0*dt/2.0d0/eps0)
    CExLy = dt/eps0/(1.0d0+sgm0*dt/2.0d0/eps0)/dy
    CExLz = dt/eps0/(1.0d0+sgm0*dt/2.0d0/eps0)/dz
    CEy = (1.0d0-sgm0*dt/2.0d0/eps0)/(1.0d0+sgm0*dt/2.0d0/eps0)
    CEyLz = dt/eps0/(1.0d0+sgm0*dt/2.0d0/eps0)/dz
    CEyLx = dt/eps0/(1.0d0+sgm0*dt/2.0d0/eps0)/dx
    CEz = (1.0d0-sgm0*dt/2.0d0/eps0)/(1.0d0+sgm0*dt/2.0d0/eps0)
    CEzLx = dt/eps0/(1.0d0+sgm0*dt/2.0d0/eps0)/dx
    CEzLy = dt/eps0/(1.0d0+sgm0*dt/2.0d0/eps0)/dy
    CHxLy = dt/mu0/dy;    CHxLz = dt/mu0/dz
    CHyLz = dt/mu0/dz;    CHyLx = dt/mu0/dx
    CHzLx = dt/mu0/dx;    CHzLy = dt/mu0/dy
    CPhi = 0.0d0; CPhi1 = 0.0d0; CPhi2 = 0.0d0; CPhi3 = 0.0d0
    
    CEx(1) = 0.0d0;    CExLy(1) = 0.0d0;    CExLz(1) = 0.0d0
    CEy(1) = 0.0d0;    CEyLz(1) = 0.0d0;    CEyLx(1) = 0.0d0
    CEz(1) = 0.0d0;    CEzLx(1) = 0.0d0;    CEzLy(1) = 0.0d0
    CHxLy(1) = 0.0d0;  CHxLz(1) = 0.0d0
    CHyLz(1) = 0.0d0;  CHyLx(1) = 0.0d0
    CHzLx(1) = 0.0d0;  CHzLy(1) = 0.0d0
    
    CEx(-1) = 1.0d0;   CExLy(-1) = 0.0d0;   CExLz(-1) = 0.0d0
    CEy(-1) = 1.0d0;   CEyLz(-1) = 0.0d0;   CEyLx(-1) = 0.0d0
    CEz(-1) = 1.0d0;   CEzLx(-1) = 0.0d0;   CEzLy(-1) = 0.0d0
    CHxLy(-1) = 0.0d0;  CHxLz(-1) = 0.0d0
    CHyLz(-1) = 0.0d0;  CHyLx(-1) = 0.0d0
    CHzLx(-1) = 0.0d0;  CHzLy(-1) = 0.0d0
    
    It = 0.0d0;  Vt = 0.0d0
    If_imp = dcmplx(0.0d0, 0.0d0);  Vf_imp = dcmplx(0.0d0, 0.0d0)
    If_pat = dcmplx(0.0d0, 0.0d0);  Vf_pat = dcmplx(0.0d0, 0.0d0)
    if((patternFlag /= 0) .or. (efficiencyFlag /= 0)) then
       ZSL_pat = nzS(id)
       ZEL_pat = nzE(id)
       if(nzS(id) <= ZS_pat) then
          ZSL_pat = ZS_pat
       end if
       if(ZE_pat <= nzE(id)) then
          ZEL_pat = ZE_pat
       end if
       if(ZSL_pat <= ZEL_pat) then
          allocate(Ey1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Ez1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Hy1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Hz1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Ey2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Ez2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Hy2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Hz2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Ez3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Ex3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Hz3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Hx3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Ez4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Ex4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Hz4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          allocate(Hx4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,1:freqPoint_pat))
          Ey1 = dcmplx(0.0d0,0.0d0);  Ez1 = dcmplx(0.0d0,0.0d0)
          Hy1 = dcmplx(0.0d0,0.0d0);  Hz1 = dcmplx(0.0d0,0.0d0)
          Ey2 = dcmplx(0.0d0,0.0d0);  Ez2 = dcmplx(0.0d0,0.0d0)
          Hy2 = dcmplx(0.0d0,0.0d0);  Hz2 = dcmplx(0.0d0,0.0d0)
          Ez3 = dcmplx(0.0d0,0.0d0);  Ex3 = dcmplx(0.0d0,0.0d0)
          Hz3 = dcmplx(0.0d0,0.0d0);  Hx3 = dcmplx(0.0d0,0.0d0)
          Ez4 = dcmplx(0.0d0,0.0d0);  Ex4 = dcmplx(0.0d0,0.0d0)
          Hz4 = dcmplx(0.0d0,0.0d0);  Hx4 = dcmplx(0.0d0,0.0d0)
       end if
       if(nzS(id) <= ZS_pat .and. ZS_pat <= nzE(id)) then
          allocate(Ex5(XS_pat:XE_pat,YS_pat:YE_pat,1:freqPoint_pat))
          allocate(Ey5(XS_pat:XE_pat,YS_pat:YE_pat,1:freqPoint_pat))
          allocate(Hx5(XS_pat:XE_pat,YS_pat:YE_pat,1:freqPoint_pat))
          allocate(Hy5(XS_pat:XE_pat,YS_pat:YE_pat,1:freqPoint_pat))
          Ex5 = dcmplx(0.0d0,0.0d0);  Ey5 = dcmplx(0.0d0,0.0d0)
          Hx5 = dcmplx(0.0d0,0.0d0);  Hy5 = dcmplx(0.0d0,0.0d0)
       end if
       if(nzS(id) <= ZE_pat+1 .and. ZE_pat+1 <= nzE(id)) then
          allocate(Ex6(XS_pat:XE_pat,YS_pat:YE_pat,1:freqPoint_pat))
          allocate(Ey6(XS_pat:XE_pat,YS_pat:YE_pat,1:freqPoint_pat))
          allocate(Hx6(XS_pat:XE_pat,YS_pat:YE_pat,1:freqPoint_pat))
          allocate(Hy6(XS_pat:XE_pat,YS_pat:YE_pat,1:freqPoint_pat))
          Ex6 = dcmplx(0.0d0,0.0d0);  Ey6 = dcmplx(0.0d0,0.0d0)
          Hx6 = dcmplx(0.0d0,0.0d0);  Hy6 = dcmplx(0.0d0,0.0d0)
       end if
    end if
    if(PLRCFlag /= 0) then
       ZSL_RC = nzS(id)
       ZEL_RC = nzE(id)
       if(nzS(id) <= ZS_RC) then
          ZSL_RC = ZS_RC
       end if
       if(ZE_RC <= nzE(id)) then
          ZEL_RC = ZE_RC
       end if
       if(ZSL_RC <= ZEL_RC) then
          allocate(ExSaveRC(XS_RC:XE_RC, YS_RC:YE_RC, ZSL_RC:ZEL_RC))
          allocate(EySaveRC(XS_RC:XE_RC, YS_RC:YE_RC, ZSL_RC:ZEL_RC))
          allocate(EzSaveRC(XS_RC:XE_RC, YS_RC:YE_RC, ZSL_RC:ZEL_RC))
          allocate(PHIx(XS_RC:XE_RC, YS_RC:YE_RC, ZSL_RC:ZEL_RC))
          allocate(PHIy(XS_RC:XE_RC, YS_RC:YE_RC, ZSL_RC:ZEL_RC))
          allocate(PHIz(XS_RC:XE_RC, YS_RC:YE_RC, ZSL_RC:ZEL_RC))
          ExSaveRC = 0.0d0; EySaveRC = 0.0d0; EzSaveRC = 0.0d0
          PHIx = 0.0d0; PHIy = 0.0d0; PHIz = 0.0d0
       end if
    end if
    
    if(CDFlag /= 0) then
       if(ZS_CD /= ZE_CD) then
          ZSL_CD = nzS(id)
          ZEL_CD = nzE(id)
          if(nzS(id) <= ZS_CD) then
             ZSL_CD = ZS_CD
          end if
          if(ZE_CD-1 <= nzE(id)) then
             ZEL_CD = ZE_CD-1
          end if
       end if
       if((XS_CD == XE_CD).and.(YS_CD /= YE_CD).and.(ZS_CD /= ZE_CD)) then
          if(ZSL_CD <= ZEL_CD) then
             allocate(Jy(YS_CD:YE_CD-1, ZSL_CD:ZEL_CD, 1:freqPoint_CD))
             allocate(Jz(YS_CD:YE_CD-1, ZSL_CD:ZEL_CD, 1:freqPoint_CD))
             Jy = dcmplx(0.0d0, 0.0d0); Jz = dcmplx(0.0d0, 0.0d0)
          end if
       elseif((XS_CD /= XE_CD).and.(YS_CD == YE_CD).and.(ZS_CD /= ZE_CD)) then
          if(ZSL_CD <= ZEL_CD) then
             allocate(Jx(XS_CD:XE_CD-1, ZSL_CD:ZEL_CD, 1:freqPoint_CD))
             allocate(Jz(XS_CD:XE_CD-1, ZSL_CD:ZEL_CD, 1:freqPoint_CD))
             Jx = dcmplx(0.0d0, 0.0d0); Jz = dcmplx(0.0d0, 0.0d0)
          end if
       elseif((XS_CD /= XE_CD).and.(YS_CD /= YE_CD).and.(ZS_CD == ZE_CD)) then
          if(nzS(id) <= ZS_CD .and. ZS_CD <= nzE(id)) then
             allocate(Jx(XS_CD:XE_CD-1, YS_CD:YE_CD-1, 1:freqPoint_CD))
             allocate(Jy(XS_CD:XE_CD-1, YS_CD:YE_CD-1, 1:freqPoint_CD))
             Jx = dcmplx(0.0d0, 0.0d0); Jy = dcmplx(0.0d0, 0.0d0)
          end if
       elseif((YS_CD == YE_CD).and.(ZS_CD == ZE_CD)) then
          if(nzS(id) <= ZS_CD .and. ZS_CD <= nzE(id)) then
             allocate(Jl(XS_CD:XE_CD-1, 1:freqPoint_CD))
             Jl = dcmplx(0.0d0, 0.0d0)
          end if
       elseif((XS_CD == XE_CD).and.(ZS_CD == ZE_CD)) then
          if(nzS(id) <= ZS_CD .and. ZS_CD <= nzE(id)) then
             allocate(Jl(YS_CD:YE_CD-1, 1:freqPoint_CD))
             Jl = dcmplx(0.0d0, 0.0d0)
          end if
       elseif((XS_CD == XE_CD).and.(YS_CD == YE_CD)) then
          if(ZSL_CD <= ZEL_CD) then
             allocate(Jl(ZSL_CD:ZEL_CD, 1:freqPoint_CD))
             Jl = dcmplx(0.0d0, 0.0d0)
          end if
       end if
    end if
    if(EDFlag /= 0) then
       if(ZS_ED /= ZE_ED) then
          ZSL_ED = nzS(id)
          ZEL_ED = nzE(id)
          if(nzS(id) <= ZS_ED) then
             ZSL_ED = ZS_ED
          end if
          if(ZE_ED-1 <= nzE(id)) then
             ZEL_ED = ZE_ED-1
          end if
       end if
       if(XS_ED == XE_ED) then
          if(ZSL_ED <= ZEL_ED) then
             allocate(Ey_ED(YS_ED:YE_ED-1, ZSL_ED:ZEL_ED, 1:freqPoint_ED))
             allocate(Ez_ED(YS_ED:YE_ED-1, ZSL_ED:ZEL_ED, 1:freqPoint_ED))
             Ey_ED = dcmplx(0.0d0, 0.0d0); Ez_ED = dcmplx(0.0d0, 0.0d0)
          end if
       else if(YS_ED == YE_ED) then
          if(ZSL_ED <= ZEL_ED) then
             allocate(Ex_ED(XS_ED:XE_ED-1, ZSL_ED:ZEL_ED, 1:freqPoint_ED))
             allocate(Ez_ED(XS_ED:XE_ED-1, ZSL_ED:ZEL_ED, 1:freqPoint_ED))
             Ex_ED = dcmplx(0.0d0, 0.0d0); Ez_ED = dcmplx(0.0d0, 0.0d0)
          end if
       else if(ZS_ED == ZE_ED) then
          if(nzS(id) <= ZS_ED .and. ZS_ED <= nzE(id)) then
             allocate(Ex_ED(XS_ED:XE_ED-1, YS_ED:YE_ED-1, 1:freqPoint_ED))
             allocate(Ey_ED(XS_ED:XE_ED-1, YS_ED:YE_ED-1, 1:freqPoint_ED))
             Ex_ED = dcmplx(0.0d0, 0.0d0); Ey_ED = dcmplx(0.0d0, 0.0d0)
          end if
       end if
    end if
  end subroutine initialize
  subroutine calc_feed
    integer :: i,x,y,z
    
    do i=1, nfeed
       select case(feedType(i))
       case(0)
          Vt(i) = gauss_pulse(dt*dble(t), feedTAU(i), feedALPHA(i), feedAMP(i))
       case(1)
          Vt(i) = d_gauss_pulse(dt*dble(t), feedTAU(i), feedALPHA(i), feedAMP(i))
       case default
       end select
       Vt(i) = Vt(i) - feedR(i)*It(i)
       select case (feedAxis(i))
       case('x')
          if((nzS(id) <= feedz(i)) .and. (feedz(i) <= nzE(id))) then
             do x=feedx(i), feedx(i)+(feedLength(i)-1)*feedSign(i), feedSign(i)
                Ex(x,feedy(i),feedz(i)) &
                     & = dble(-feedSign(i))*Vt(i)/dx/dble(feedLength(i))
             end do
          end if
       case('y')
          if((nzS(id) <= feedz(i)) .and. (feedz(i) <= nzE(id))) then
             do y=feedy(i), feedy(i)+(feedLength(i)-1)*feedSign(i), feedSign(i)
                Ey(feedx(i),y,feedz(i)) &
                     & = dble(-feedSign(i))*Vt(i)/dy/dble(feedLength(i))
             end do
          end if
       case('z')
          do z=feedz(i), feedz(i)+(feedLength(i)-1)*feedSign(i), feedSign(i)
             if((nzS(id) <= z) .and. (z <= nzE(id))) then
                Ez(feedx(i),feedy(i),z) &
                     & = dble(-feedSign(i))*Vt(i)/dz/dble(feedLength(i))
             end if
          end do
       end select
    enddo
  end subroutine calc_feed
  subroutine calc_E(k)
    integer :: i, j, k
    if(k <= LM) then
       do j=2, ny
          do i=2, nx
             Ezx1(i,j,k) = CpmlEdx(i)*Ezx1(i,j,k) &
                  & + CpmlEdxL(i)*(Hy(i,j,k)-Hy(i-1,j,k))
             Ezy1(i,j,k) = CpmlEdy(j)*Ezy1(i,j,k) &
                  & + CpmlEdyL(j)*(Hx(i,j-1,k)-Hx(i,j,k))
             Ez(i,j,k) = Ezx1(i,j,k) + Ezy1(i,j,k)
          enddo
       enddo
       if(k >= 2) then
          do j=1, ny
             do i=2, nx
                Eyx1(i,j,k) = CpmlEdx(i)*Eyx1(i,j,k) &
                     & + CpmlEdxL(i)*(Hz(i-1,j,k)-Hz(i,j,k))
                Eyz1(i,j,k) = CpmlEdz(k)*Eyz1(i,j,k) &
                     & + CpmlEdzL(k)*(Hx(i,j,k)-Hx(i,j,k-1))
                Ey(i,j,k) = Eyx1(i,j,k) + Eyz1(i,j,k)
             enddo
          enddo
          do j=2, ny
             do i=1, nx
                Exy1(i,j,k) = CpmlEdy(j)*Exy1(i,j,k) &
                     & + CpmlEdyL(j)*(Hz(i,j,k)-Hz(i,j-1,k))
                Exz1(i,j,k) = CpmlEdz(k)*Exz1(i,j,k) &
                     & + CpmlEdzL(k)*(Hy(i,j,k-1)-Hy(i,j,k))
                Ex(i,j,k) = Exy1(i,j,k) + Exz1(i,j,k)
             enddo
          enddo
       endif
    else if(k >= nz-LM+1) then
       do j=2, ny
          do i=2, nx
             Ezx2(i,j,k) = CpmlEdx(i)*Ezx2(i,j,k) &
                  & + CpmlEdxL(i)*(Hy(i,j,k)-Hy(i-1,j,k))
             Ezy2(i,j,k) = CpmlEdy(j)*Ezy2(i,j,k) &
                  & + CpmlEdyL(j)*(Hx(i,j-1,k)-Hx(i,j,k))
             Ez(i,j,k) = Ezx2(i,j,k) + Ezy2(i,j,k)
          enddo
       enddo
       do j=1, ny
          do i=2, nx
             Eyx2(i,j,k) = CpmlEdx(i)*Eyx2(i,j,k) &
                  & + CpmlEdxL(i)*(Hz(i-1,j,k)-Hz(i,j,k))
             Eyz2(i,j,k) = CpmlEdz(k)*Eyz2(i,j,k) &
                  & + CpmlEdzL(k)*(Hx(i,j,k)-Hx(i,j,k-1))
             Ey(i,j,k) = Eyx2(i,j,k) + Eyz2(i,j,k)
          enddo
       enddo
       do j=2, ny
          do i=1, nx
             Exy2(i,j,k) = CpmlEdy(j)*Exy2(i,j,k) &
                  & + CpmlEdyL(j)*(Hz(i,j,k)-Hz(i,j-1,k))
             Exz2(i,j,k) = CpmlEdz(k)*Exz2(i,j,k) &
                  & + CpmlEdzL(k)*(Hy(i,j,k-1)-Hy(i,j,k))
             Ex(i,j,k) = Exy2(i,j,k) + Exz2(i,j,k)
          enddo
       enddo
    else
       do j=2, LM
          do i=2, nx
             Ezx3(i,j,k) = CpmlEdx(i)*Ezx3(i,j,k) &
                  & + CpmlEdxL(i)*(Hy(i,j,k)-Hy(i-1,j,k))
             Ezy3(i,j,k) = CpmlEdy(j)*Ezy3(i,j,k) &
                  & + CpmlEdyL(j)*(Hx(i,j-1,k)-Hx(i,j,k))
             Ez(i,j,k) = Ezx3(i,j,k) + Ezy3(i,j,k)
          enddo
       enddo
       do j=LM+1, ny-LM
          do i=2, LM
             Ezx5(i,j,k) = CpmlEdx(i)*Ezx5(i,j,k) &
                  & + CpmlEdxL(i)*(Hy(i,j,k)-Hy(i-1,j,k))
             Ezy5(i,j,k) = CpmlEdy(j)*Ezy5(i,j,k) &
                  & + CpmlEdyL(j)*(Hx(i,j-1,k)-Hx(i,j,k))
             Ez(i,j,k) = Ezx5(i,j,k) + Ezy5(i,j,k)
          enddo
          do i=LM+1, nx-LM
             Ez(i,j,k) = CEz(IDEz(i,j,k))*Ez(i,j,k) &
                  & + CEzLx(IDEz(i,j,k))*(Hy(i,j,k)-Hy(i-1,j,k)) &
                  & - CEzLy(IDEz(i,j,k))*(Hx(i,j,k)-Hx(i,j-1,k))
          enddo
          do i=nx-LM+1, nx
             Ezx6(i,j,k) = CpmlEdx(i)*Ezx6(i,j,k) &
                  & + CpmlEdxL(i)*(Hy(i,j,k)-Hy(i-1,j,k))
             Ezy6(i,j,k) = CpmlEdy(j)*Ezy6(i,j,k) &
                  & + CpmlEdyL(j)*(Hx(i,j-1,k)-Hx(i,j,k))
             Ez(i,j,k) = Ezx6(i,j,k) + Ezy6(i,j,k)
          enddo
       enddo
       do j=ny-LM+1, ny
          do i=2, nx
             Ezx4(i,j,k) = CpmlEdx(i)*Ezx4(i,j,k) &
                  & + CpmlEdxL(i)*(Hy(i,j,k)-Hy(i-1,j,k))
             Ezy4(i,j,k) = CpmlEdy(j)*Ezy4(i,j,k) &
                  & + CpmlEdyL(j)*(Hx(i,j-1,k)-Hx(i,j,k))
             Ez(i,j,k) = Ezx4(i,j,k) + Ezy4(i,j,k)
          enddo
       enddo
       do j=1, LM
          do i=2, nx
             Eyx3(i,j,k) = CpmlEdx(i)*Eyx3(i,j,k) &
                  & + CpmlEdxL(i)*(Hz(i-1,j,k)-Hz(i,j,k))
             Eyz3(i,j,k) = CpmlEdz(k)*Eyz3(i,j,k) &
                  & + CpmlEdzL(k)*(Hx(i,j,k)-Hx(i,j,k-1))
             Ey(i,j,k) = Eyx3(i,j,k) + Eyz3(i,j,k)
          enddo
       enddo
       do j=LM+1, ny-LM
          do i=2, LM
             Eyx5(i,j,k) = CpmlEdx(i)*Eyx5(i,j,k) &
                  & + CpmlEdxL(i)*(Hz(i-1,j,k)-Hz(i,j,k))
             Eyz5(i,j,k) = CpmlEdz(k)*Eyz5(i,j,k) &
                  & + CpmlEdzL(k)*(Hx(i,j,k)-Hx(i,j,k-1))
             Ey(i,j,k) = Eyx5(i,j,k) + Eyz5(i,j,k)
          enddo
          do i=LM+1, nx-LM
             Ey(i,j,k) = CEy(IDEy(i,j,k))*Ey(i,j,k) &
                  & + CEyLz(IDEy(i,j,k))*(Hx(i,j,k)-Hx(i,j,k-1)) &
                  & - CEyLx(IDEy(i,j,k))*(Hz(i,j,k)-Hz(i-1,j,k))
          enddo
          do i=nx-LM+1, nx
             Eyx6(i,j,k) = CpmlEdx(i)*Eyx6(i,j,k) &
                  & + CpmlEdxL(i)*(Hz(i-1,j,k)-Hz(i,j,k))
             Eyz6(i,j,k) = CpmlEdz(k)*Eyz6(i,j,k) &
                  & + CpmlEdzL(k)*(Hx(i,j,k)-Hx(i,j,k-1))
             Ey(i,j,k) = Eyx6(i,j,k) + Eyz6(i,j,k)
          enddo
       enddo
       do j=ny-LM+1, ny
          do i=2, nx
             Eyx4(i,j,k) = CpmlEdx(i)*Eyx4(i,j,k) &
                  & + CpmlEdxL(i)*(Hz(i-1,j,k)-Hz(i,j,k))
             Eyz4(i,j,k) = CpmlEdz(k)*Eyz4(i,j,k) &
                  & + CpmlEdzL(k)*(Hx(i,j,k)-Hx(i,j,k-1))
             Ey(i,j,k) = Eyx4(i,j,k) + Eyz4(i,j,k)
          enddo
       enddo
       do j=2, LM
          do i=1, nx
             Exy3(i,j,k) = CpmlEdy(j)*Exy3(i,j,k) &
                  & + CpmlEdyL(j)*(Hz(i,j,k)-Hz(i,j-1,k))
             Exz3(i,j,k) = CpmlEdz(k)*Exz3(i,j,k) &
                  & + CpmlEdzL(k)*(Hy(i,j,k-1)-Hy(i,j,k))
             Ex(i,j,k) = Exy3(i,j,k) + Exz3(i,j,k)
          enddo
       enddo
       do j=LM+1, ny-LM
          do i=1, LM
             Exy5(i,j,k) = CpmlEdy(j)*Exy5(i,j,k) &
                  & + CpmlEdyL(j)*(Hz(i,j,k)-Hz(i,j-1,k))
             Exz5(i,j,k) = CpmlEdz(k)*Exz5(i,j,k) &
                  & + CpmlEdzL(k)*(Hy(i,j,k-1)-Hy(i,j,k))
             Ex(i,j,k) = Exy5(i,j,k) + Exz5(i,j,k)
          enddo
          do i=LM+1, nx-LM
             Ex(i,j,k) = CEx(IDEx(i,j,k))*Ex(i,j,k) &
                  & + CExLy(IDEx(i,j,k))*(Hz(i,j,k)-Hz(i,j-1,k)) &
                  & - CExLz(IDEx(i,j,k))*(Hy(i,j,k)-Hy(i,j,k-1))
          enddo
          do i=nx-LM+1, nx
             Exy6(i,j,k) = CpmlEdy(j)*Exy6(i,j,k) &
                  & + CpmlEdyL(j)*(Hz(i,j,k)-Hz(i,j-1,k))
             Exz6(i,j,k) = CpmlEdz(k)*Exz6(i,j,k) &
                  & + CpmlEdzL(k)*(Hy(i,j,k-1)-Hy(i,j,k))
             Ex(i,j,k) = Exy6(i,j,k) + Exz6(i,j,k)
          enddo
       enddo
       do j=ny-LM+1, ny
          do i=1, nx
             Exy4(i,j,k) = CpmlEdy(j)*Exy4(i,j,k) &
                  & + CpmlEdyL(j)*(Hz(i,j,k)-Hz(i,j-1,k))
             Exz4(i,j,k) = CpmlEdz(k)*Exz4(i,j,k) &
                  & + CpmlEdzL(k)*(Hy(i,j,k-1)-Hy(i,j,k))
             Ex(i,j,k) = Exy4(i,j,k) + Exz4(i,j,k)
          enddo
       enddo
    endif
  end subroutine calc_E
  
  subroutine calc_PHI(k)
    integer :: i,j,k
    
    if(ZSL_RC <= k .and. k <= ZEL_RC) then
       do j=YS_RC, YE_RC
          do i=XS_RC, XE_RC
             PHIx(i,j,k) = CPhi1(IDEx(i,j,k))*Ex(i,j,k) &
                  & + CPhi2(IDEx(i,j,k))*ExSaveRC(i,j,k) &
                  & + CPhi3(IDEx(i,j,k))*PHIx(i,j,k)
             ExSaveRC(i,j,k) = Ex(i,j,k)
             PHIy(i,j,k) = CPhi1(IDEy(i,j,k))*Ey(i,j,k) &
                  & + CPhi2(IDEy(i,j,k))*EySaveRC(i,j,k) &
                  & + CPhi3(IDEy(i,j,k))*PHIy(i,j,k)
             EySaveRC(i,j,k) = Ey(i,j,k)
             PHIz(i,j,k) = CPhi1(IDEz(i,j,k))*Ez(i,j,k) &
                  & + CPhi2(IDEz(i,j,k))*EzSaveRC(i,j,k) &
                  & + CPhi3(IDEz(i,j,k))*PHIz(i,j,k)
             EzSaveRC(i,j,k) = Ez(i,j,k)
          end do
       end do
    end if
  end subroutine calc_PHI
  subroutine E_PHI_update(k)
    integer :: i,j,k
    
    if(ZSL_RC <= k .and. k <= ZEL_RC) then
       do j=YS_RC, YE_RC
          do i=XS_RC, XE_RC
             Ex(i,j,k) = Ex(i,j,k) + CPhi(IDEx(i,j,k))*PHIx(i,j,k)
             Ey(i,j,k) = Ey(i,j,k) + CPhi(IDEy(i,j,k))*PHIy(i,j,k)
             Ez(i,j,k) = Ez(i,j,k) + CPhi(IDEz(i,j,k))*PHIz(i,j,k)
          end do
       end do
    end if
  end subroutine E_PHI_update
  
  subroutine calc_H(k)
    integer :: i, j, k
    if(k <= LM) then
       do j=ny-1, 1, -1
          do i=nx-1, 1, -1
             Hzx1(i,j,k) = CpmlHdx(i)*Hzx1(i,j,k) &
                  & + CpmlHdxL(i)*(Ey(i,j,k)-Ey(i+1,j,k))
             Hzy1(i,j,k) = CpmlHdy(j)*Hzy1(i,j,k) &
                  & + CpmlHdyL(j)*(Ex(i,j+1,k)-Ex(i,j,k))
             Hz(i,j,k) = Hzx1(i,j,k) + Hzy1(i,j,k)
          enddo
       enddo
       do j=ny, 1, -1
          do i=nx-1, 1, -1
             Hyz1(i,j,k) = CpmlHdz(k)*Hyz1(i,j,k) &
                  & + CpmlHdzL(k)*(Ex(i,j,k)-Ex(i,j,k+1))
             Hyx1(i,j,k) = CpmlHdx(i)*Hyx1(i,j,k) &
                  & + CpmlHdxL(i)*(Ez(i+1,j,k)-Ez(i,j,k))
             Hy(i,j,k) = Hyz1(i,j,k) + Hyx1(i,j,k)
          enddo
       enddo
       do j=ny-1, 1, -1
          do i=nx, 1, -1
             Hxy1(i,j,k) = CpmlHdy(j)*Hxy1(i,j,k) &
                  & + CpmlHdyL(j)*(Ez(i,j,k)-Ez(i,j+1,k))
             Hxz1(i,j,k) = CpmlHdz(k)*Hxz1(i,j,k) &
                  & + CpmlHdzL(k)*(Ey(i,j,k+1)-Ey(i,j,k))
             Hx(i,j,k) = Hxy1(i,j,k) + Hxz1(i,j,k)
          enddo
       enddo
    else if(k >= nz-LM+1) then
       do j=ny-1, 1, -1
          do i=nx-1, 1, -1
             Hzx2(i,j,k) = CpmlHdx(i)*Hzx2(i,j,k) &
                  & + CpmlHdxL(i)*(Ey(i,j,k)-Ey(i+1,j,k))
             Hzy2(i,j,k) = CpmlHdy(j)*Hzy2(i,j,k) &
                  & + CpmlHdyL(j)*(Ex(i,j+1,k)-Ex(i,j,k))
             Hz(i,j,k) = Hzx2(i,j,k) + Hzy2(i,j,k)
          enddo
       enddo
       if(k <= nz-1) then
          do j=ny, 1, -1
             do i=nx-1, 1, -1
                Hyz2(i,j,k) = CpmlHdz(k)*Hyz2(i,j,k) &
                     & + CpmlHdzL(k)*(Ex(i,j,k)-Ex(i,j,k+1))
                Hyx2(i,j,k) = CpmlHdx(i)*Hyx2(i,j,k) &
                     & + CpmlHdxL(i)*(Ez(i+1,j,k)-Ez(i,j,k))
                Hy(i,j,k) = Hyz2(i,j,k) + Hyx2(i,j,k)
             enddo
          enddo
          do j=ny-1, 1, -1
             do i=nx, 1, -1
                Hxy2(i,j,k) = CpmlHdy(j)*Hxy2(i,j,k) &
                     & + CpmlHdyL(j)*(Ez(i,j,k)-Ez(i,j+1,k))
                Hxz2(i,j,k) = CpmlHdz(k)*Hxz2(i,j,k) &
                     & + CpmlHdzL(k)*(Ey(i,j,k+1)-Ey(i,j,k))
                Hx(i,j,k) = Hxy2(i,j,k) + Hxz2(i,j,k)
             enddo
          enddo
       endif
    else
       do j=ny-1, ny-LM+1, -1
          do i=nx-1, 1, -1
             Hzx4(i,j,k) = CpmlHdx(i)*Hzx4(i,j,k) &
                  & + CpmlHdxL(i)*(Ey(i,j,k)-Ey(i+1,j,k))
             Hzy4(i,j,k) = CpmlHdy(j)*Hzy4(i,j,k) &
                  & + CpmlHdyL(j)*(Ex(i,j+1,k)-Ex(i,j,k))
             Hz(i,j,k) = Hzx4(i,j,k) + Hzy4(i,j,k)
          enddo
       enddo
       do j=ny-LM, LM+1, -1
          do i=nx-1, nx-LM+1, -1
             Hzx6(i,j,k) = CpmlHdx(i)*Hzx6(i,j,k) &
                  & + CpmlHdxL(i)*(Ey(i,j,k)-Ey(i+1,j,k))
             Hzy6(i,j,k) = CpmlHdy(j)*Hzy6(i,j,k) &
                  & + CpmlHdyL(j)*(Ex(i,j+1,k)-Ex(i,j,k))
             Hz(i,j,k) = Hzx6(i,j,k) + Hzy6(i,j,k)
          enddo
          do i=nx-LM, LM+1, -1
             Hz(i,j,k) = Hz(i,j,k) &
                  & - CHzLx(IDHy(i,j,k))*(Ey(i+1,j,k)-Ey(i,j,k)) &
                  & + CHzLy(IDHy(i,j,k))*(Ex(i,j+1,k)-Ex(i,j,k))
          enddo
          do i=LM, 1, -1
             Hzx5(i,j,k) = CpmlHdx(i)*Hzx5(i,j,k) &
                  & + CpmlHdxL(i)*(Ey(i,j,k)-Ey(i+1,j,k))
             Hzy5(i,j,k) = CpmlHdy(j)*Hzy5(i,j,k) &
                  & + CpmlHdyL(j)*(Ex(i,j+1,k)-Ex(i,j,k))
             Hz(i,j,k) = Hzx5(i,j,k) + Hzy5(i,j,k)
          enddo
       enddo
       do j=LM, 1, -1
          do i=nx-1, 1, -1
             Hzx3(i,j,k) = CpmlHdx(i)*Hzx3(i,j,k) &
                  & + CpmlHdxL(i)*(Ey(i,j,k)-Ey(i+1,j,k))
             Hzy3(i,j,k) = CpmlHdy(j)*Hzy3(i,j,k) &
                  & + CpmlHdyL(j)*(Ex(i,j+1,k)-Ex(i,j,k))
             Hz(i,j,k) = Hzx3(i,j,k) + Hzy3(i,j,k)
          enddo
       enddo
       do j=ny, ny-LM+1, -1
          do i=nx-1, 1, -1
             Hyz4(i,j,k) = CpmlHdz(k)*Hyz4(i,j,k) &
                  & + CpmlHdzL(k)*(Ex(i,j,k)-Ex(i,j,k+1))
             Hyx4(i,j,k) = CpmlHdx(i)*Hyx4(i,j,k) &
                  & + CpmlHdxL(i)*(Ez(i+1,j,k)-Ez(i,j,k))
             Hy(i,j,k) = Hyz4(i,j,k) + Hyx4(i,j,k)
          enddo
       enddo
       do j=ny-LM, LM+1, -1
          do i=nx-1, nx-LM+1, -1
             Hyz6(i,j,k) = CpmlHdz(k)*Hyz6(i,j,k) &
                  & + CpmlHdzL(k)*(Ex(i,j,k)-Ex(i,j,k+1))
             Hyx6(i,j,k) = CpmlHdx(i)*Hyx6(i,j,k) &
                  & + CpmlHdxL(i)*(Ez(i+1,j,k)-Ez(i,j,k))
             Hy(i,j,k) = Hyz6(i,j,k) + Hyx6(i,j,k)
          enddo
          do i=nx-LM, LM+1, -1
             Hy(i,j,k) = Hy(i,j,k) &
                  & - CHyLz(IDHy(i,j,k))*(Ex(i,j,k+1)-Ex(i,j,k)) &
                  & + CHyLx(IDHy(i,j,k))*(Ez(i+1,j,k)-Ez(i,j,k))
          enddo
          do i=LM, 1, -1
             Hyz5(i,j,k) = CpmlHdz(k)*Hyz5(i,j,k) &
                  & + CpmlHdzL(k)*(Ex(i,j,k)-Ex(i,j,k+1))
             Hyx5(i,j,k) = CpmlHdx(i)*Hyx5(i,j,k) &
                  & + CpmlHdxL(i)*(Ez(i+1,j,k)-Ez(i,j,k))
             Hy(i,j,k) = Hyz5(i,j,k) + Hyx5(i,j,k)
          enddo
       enddo
       do j=LM, 1, -1
          do i=nx-1, 1, -1
             Hyz3(i,j,k) = CpmlHdz(k)*Hyz3(i,j,k) &
                  & + CpmlHdzL(k)*(Ex(i,j,k)-Ex(i,j,k+1))
             Hyx3(i,j,k) = CpmlHdx(i)*Hyx3(i,j,k) &
                  & + CpmlHdxL(i)*(Ez(i+1,j,k)-Ez(i,j,k))
             Hy(i,j,k) = Hyz3(i,j,k) + Hyx3(i,j,k)
          enddo
       enddo
       do j=ny-1, ny-LM+1, -1
          do i=nx, 1, -1
             Hxy4(i,j,k) = CpmlHdy(j)*Hxy4(i,j,k) &
                  & + CpmlHdyL(j)*(Ez(i,j,k)-Ez(i,j+1,k))
             Hxz4(i,j,k) = CpmlHdz(k)*Hxz4(i,j,k) &
                  & + CpmlHdzL(k)*(Ey(i,j,k+1)-Ey(i,j,k))
             Hx(i,j,k) = Hxy4(i,j,k) + Hxz4(i,j,k)
          enddo
       enddo
       do j=ny-LM, LM+1, -1
          do i=nx, nx-LM+1, -1
             Hxy6(i,j,k) = CpmlHdy(j)*Hxy6(i,j,k) &
                  & + CpmlHdyL(j)*(Ez(i,j,k)-Ez(i,j+1,k))
             Hxz6(i,j,k) = CpmlHdz(k)*Hxz6(i,j,k) &
                  & + CpmlHdzL(k)*(Ey(i,j,k+1)-Ey(i,j,k))
             Hx(i,j,k) = Hxy6(i,j,k) + Hxz6(i,j,k)
          enddo
          do i=nx-LM, LM+1, -1
             Hx(i,j,k) = Hx(i,j,k) &
                  & - CHxLy(IDHx(i,j,k))*(Ez(i,j+1,k)-Ez(i,j,k)) &
                  & + CHxLz(IDHx(i,j,k))*(Ey(i,j,k+1)-Ey(i,j,k))
          enddo
          do i=LM, 1, -1
             Hxy5(i,j,k) = CpmlHdy(j)*Hxy5(i,j,k) &
                  & + CpmlHdyL(j)*(Ez(i,j,k)-Ez(i,j+1,k))
             Hxz5(i,j,k) = CpmlHdz(k)*Hxz5(i,j,k) &
                  & + CpmlHdzL(k)*(Ey(i,j,k+1)-Ey(i,j,k))
             Hx(i,j,k) = Hxy5(i,j,k) + Hxz5(i,j,k)
          enddo
       enddo
       do j=LM, 1, -1
          do i=nx, 1, -1
             Hxy3(i,j,k) = CpmlHdy(j)*Hxy3(i,j,k) &
                  & + CpmlHdyL(j)*(Ez(i,j,k)-Ez(i,j+1,k))
             Hxz3(i,j,k) = CpmlHdz(k)*Hxz3(i,j,k) &
                  & + CpmlHdzL(k)*(Ey(i,j,k+1)-Ey(i,j,k))
             Hx(i,j,k) = Hxy3(i,j,k) + Hxz3(i,j,k)
          enddo
       enddo
    endif
  end subroutine calc_H
  
  subroutine calc_H_CP(k)
    integer :: i,j,k,n
    
    do n = cpS(k), cpE(k)
       i = cpX(n)
       j = cpY(n)
       select case(cpPlaneFlag(n))
       case (1)
          Hx(i,j,k) = Hx(i,j,k) &
               & + CHxLy(IDHx(i,j,k))*(Ez(i,j+1,k)-Ez(i,j,k)) &
               & - CHxLz(IDHx(i,j,k))*(Ey(i,j,k+1)-Ey(i,j,k))
          Hx(i,j,k) = Hx(i,j,k) &
               & - CHCP(n)*(Ez(i,j+1,k)*llz2(n) - Ez(i,j,k)*llz1(n) &
               & - Ey(i,j,k+1)*lly2(n) + Ey(i,j,k)*lly1(n))
       case (2)
          Hy(i,j,k) = Hy(i,j,k) &
               & + CHyLz(IDHy(i,j,k))*(Ex(i,j,k+1)-Ex(i,j,k)) &
               & - CHyLx(IDHy(i,j,k))*(Ez(i+1,j,k)-Ez(i,j,k))
          Hy(i,j,k) = Hy(i,j,k) &
               & - CHCP(n)*(Ex(i,j,k+1)*llx2(n) - Ex(i,j,k)*llx1(n) &
               & - Ez(i+1,j,k)*llz2(n) + Ez(i,j,k)*llz1(n))
       case (3)
          Hz(i,j,k) = Hz(i,j,k) &
               & + CHzLx(IDHy(i,j,k))*(Ey(i+1,j,k)-Ey(i,j,k))&
               & - CHzLy(IDHy(i,j,k))*(Ex(i,j+1,k)-Ex(i,j,k))
          Hz(i,j,k) = Hz(i,j,k) &
               & - CHCP(n)*(Ey(i+1,j,k)*lly2(n) - Ey(i,j,k)*lly1(n) &
               & - Ex(i,j+1,k)*llx2(n) + Ex(i,j,k)*llx1(n))
       end select
    end do
  end subroutine calc_H_CP
  
  subroutine calc_surface_Jm
    integer :: f
    complex*16 :: CE
    real*8 :: m, span
    
    if(freqPoint_pat /= 1) then
       span = (freqE_pat - freqS_pat)/dble(freqPoint_pat - 1)/df
    end if
    if(ZSL_pat <= ZEL_pat) then
       m = freqS_pat/df
       do f = 1, freqPoint_pat
          CE = 0.5d0*dt*exp(dcmplx(0.0,-2.0)*pi*m*dble(t)/dble(nt))
          Ey1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & = Ey1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & + CE*(Ey(XS_pat,YS_pat:YE_pat,ZSL_pat:ZEL_pat) &
               & + Ey(XS_pat,YS_pat:YE_pat,ZSL_pat+1:ZEL_pat+1))
          Ez1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & = Ez1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & + CE*(Ez(XS_pat,YS_pat:YE_pat,ZSL_pat:ZEL_pat) &
               & + Ez(XS_pat,YS_pat+1:YE_pat+1,ZSL_pat:ZEL_pat))
          Ey2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & = Ey2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & + CE*(Ey(XE_pat+1,YS_pat:YE_pat,ZSL_pat:ZEL_pat) &
               & + Ey(XE_pat+1,YS_pat:YE_pat,ZSL_pat+1:ZEL_pat+1))
          Ez2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & = Ez2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & + CE*(Ez(XE_pat+1,YS_pat:YE_pat,ZSL_pat:ZEL_pat) &
               & + Ez(XE_pat+1,YS_pat+1:YE_pat+1,ZSL_pat:ZEL_pat))
          Ex3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & = Ex3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & + CE*(Ex(XS_pat:XE_pat,YS_pat,ZSL_pat:ZEL_pat) &
               & + Ex(XS_pat:XE_pat,YS_pat,ZSL_pat+1:ZEL_pat+1))
          Ez3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & = Ez3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & + CE*(Ez(XS_pat:XE_pat,YS_pat,ZSL_pat:ZEL_pat) &
               & + Ez(XS_pat+1:XE_pat+1,YS_pat,ZSL_pat:ZEL_pat))
          Ex4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & = Ex4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & + CE*(Ex(XS_pat:XE_pat,YE_pat+1,ZSL_pat:ZEL_pat) &
               & + Ex(XS_pat:XE_pat,YE_pat+1,ZSL_pat+1:ZEL_pat+1))
          Ez4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & = Ez4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & + CE*(Ez(XS_pat:XE_pat,YE_pat+1,ZSL_pat:ZEL_pat) &
               & + Ez(XS_pat+1:XE_pat+1,YE_pat+1,ZSL_pat:ZEL_pat))
          m = m + span
       end do
    end if
    if((nzS(id) <= ZS_pat) .and. (ZS_pat <= nzE(id))) then
       m = freqS_pat/df
       do f = 1, freqPoint_pat
          CE = 0.5d0*dt*exp(dcmplx(0.0,-2.0)*pi*m*dble(t)/dble(nt))
          Ex5(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & = Ex5(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & + CE*(Ex(XS_pat:XE_pat,YS_pat:YE_pat,ZS_pat) &
               & + Ex(XS_pat:XE_pat,YS_pat+1:YE_pat+1,ZS_pat))
          Ey5(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & = Ey5(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & + CE*(Ey(XS_pat:XE_pat,YS_pat:YE_pat,ZS_pat) &
               & + Ey(XS_pat+1:XE_pat+1,YS_pat:YE_pat,ZS_pat))
          m = m + span
       end do
    end if
    if((nzS(id) <= ZE_pat+1) .and. (ZE_pat+1 <= nzE(id))) then
       m = freqS_pat/df
       do f = 1, freqPoint_pat
          CE = 0.5d0*dt*exp(dcmplx(0.0,-2.0)*pi*m*dble(t)/dble(nt))
          Ex6(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & = Ex6(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & + CE*(Ex(XS_pat:XE_pat,YS_pat:YE_pat,ZE_pat+1) &
               & + Ex(XS_pat:XE_pat,YS_pat+1:YE_pat+1,ZE_pat+1))
          Ey6(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & = Ey6(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & + CE*(Ey(XS_pat:XE_pat,YS_pat:YE_pat,ZE_pat+1) &
               & + Ey(XS_pat+1:XE_pat+1,YS_pat:YE_pat,ZE_pat+1))
          m = m + span
       end do
    end if
  end subroutine calc_surface_Jm
  subroutine calc_surface_J
    integer :: f
    complex*16 :: CH
    real*8 :: m, span
    if(freqPoint_pat /= 1) then
       span = (freqE_pat - freqS_pat)/dble(freqPoint_pat - 1)/df
    end if
    if(ZSL_pat <= ZEL_pat) then
       m = freqS_pat/df
       do f = 1, freqPoint_pat
          CH = 0.5d0*dt*exp(dcmplx(0.0,-2.0)*pi*m*(dble(t)+0.5d0)/dble(nt))
          Hy1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & = Hy1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & + CH*(Hy(XS_pat,YS_pat:YE_pat,ZSL_pat:ZEL_pat) &
               & + Hy(XS_pat-1,YS_pat:YE_pat,ZSL_pat:ZEL_pat))
          Hz1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & = Hz1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & + CH*(Hz(XS_pat,YS_pat:YE_pat,ZSL_pat:ZEL_pat) &
               & + Hz(XS_pat-1,YS_pat:YE_pat,ZSL_pat:ZEL_pat))
          Hy2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & = Hy2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & + CH*(Hy(XE_pat+1,YS_pat:YE_pat,ZSL_pat:ZEL_pat) &
               & + Hy(XE_pat,YS_pat:YE_pat,ZSL_pat:ZEL_pat))
          Hz2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & = Hz2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & + CH*(Hz(XE_pat+1,YS_pat:YE_pat,ZSL_pat:ZEL_pat) &
               & + Hz(XE_pat,YS_pat:YE_pat,ZSL_pat:ZEL_pat))
          Hx3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & = Hx3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & + CH*(Hx(XS_pat:XE_pat,YS_pat,ZSL_pat:ZEL_pat) &
               & + Hx(XS_pat:XE_pat,YS_pat-1,ZSL_pat:ZEL_pat))
          Hz3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & = Hz3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & + CH*(Hz(XS_pat:XE_pat,YS_pat,ZSL_pat:ZEL_pat) &
               & + Hz(XS_pat:XE_pat,YS_pat-1,ZSL_pat:ZEL_pat))
          Hx4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & = Hx4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & + CH*(Hx(XS_pat:XE_pat,YE_pat+1,ZSL_pat:ZEL_pat) &
               & + Hx(XS_pat:XE_pat,YE_pat,ZSL_pat:ZEL_pat))
          Hz4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & = Hz4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & + CH*(Hz(XS_pat:XE_pat,YE_pat+1,ZSL_pat:ZEL_pat) &
               & + Hz(XS_pat:XE_pat,YE_pat,ZSL_pat:ZEL_pat))
          m = m + span
       end do
    end if
    if((nzS(id) <= ZS_pat) .and. (ZS_pat <= nzE(id))) then
       m = freqS_pat/df
       do f = 1, freqPoint_pat
          CH = 0.5d0*dt*exp(dcmplx(0.0,-2.0)*pi*m*(dble(t)+0.5d0)/dble(nt))
          Hx5(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & = Hx5(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & + CH*(Hx(XS_pat:XE_pat,YS_pat:YE_pat,ZS_pat) &
               & + Hx(XS_pat:XE_pat,YS_pat:YE_pat,ZS_pat-1))
          Hy5(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & = Hy5(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & + CH*(Hy(XS_pat:XE_pat,YS_pat:YE_pat,ZS_pat) &
               & + Hy(XS_pat:XE_pat,YS_pat:YE_pat,ZS_pat-1))
          m = m + span
       end do
    end if
    if((nzS(id) <= ZE_pat+1) .and. (ZE_pat+1 <= nzE(id))) then
       m = freqS_pat/df
       do f = 1, freqPoint_pat
          CH = 0.5d0*dt*exp(dcmplx(0.0,-2.0)*pi*m*(dble(t)+0.5d0)/dble(nt))
          Hx6(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & = Hx6(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & + CH*(Hx(XS_pat:XE_pat,YS_pat:YE_pat,ZE_pat+1) &
               & + Hx(XS_pat:XE_pat,YS_pat:YE_pat,ZE_pat))
          Hy6(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & = Hy6(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & + CH*(Hy(XS_pat:XE_pat,YS_pat:YE_pat,ZE_pat+1) &
               & + Hy(XS_pat:XE_pat,YS_pat:YE_pat,ZE_pat))
          m = m + span
       end do
    end if
  end subroutine calc_surface_J
  subroutine calc_I
    integer :: i, j
    real*8 :: It2, m, span
    do i=1, nfeed
       if(id == feedNode(i)) then
          It2 = It(i)
          select case (feedAxis(i))
          case('x')
             It(i) = dy*(-Hy(feedcx(i), feedcy(i), feedcz(i)) &
                  & + Hy(feedcx(i), feedcy(i), feedcz(i)-1)) &
                  & + dz*(Hz(feedcx(i), feedcy(i), feedcz(i)) &
                  & - Hz(feedcx(i), feedcy(i)-1, feedcz(i)))
          case('y')
             It(i) = dz*(-Hz(feedcx(i), feedcy(i), feedcz(i)) &
                  & + Hz(feedcx(i)-1, feedcy(i), feedcz(i))) &
                  & + dx*(Hx(feedcx(i), feedcy(i), feedcz(i)) &
                  & - Hx(feedcx(i), feedcy(i), feedcz(i)-1))
          case('z')
             It(i) = dx*(-Hx(feedcx(i), feedcy(i), feedcz(i)) &
                  & + Hx(feedcx(i), feedcy(i)-1, feedcz(i))) &
                  & + dy*(Hy(feedcx(i), feedcy(i), feedcz(i)) &
                  & - Hy(feedcx(i)-1, feedcy(i), feedcz(i)))
          case default
          end select
          It(i) = It(i)*dble(feedSign(i))
          It2 = (It2 + It(i))/2.0d0
          if(impedanceFlag /= 0) then
             if(freqPoint_imp /= 1) then
                span = (freqE_imp - freqS_imp)/dble(freqPoint_imp - 1)/df
             end if
             m = freqS_imp/df
             do j=1, freqPoint_imp
                If_imp(j, i) = If_imp(j, i) &
                     & + dt*It2*exp(dcmplx(0.0d0,-2.0d0)*pi*m*dble(t)/dble(nt))
                Vf_imp(j, i) = Vf_imp(j, i) &
                     & + dt*Vt(i)*exp(dcmplx(0.0d0,-2.0d0)*pi*m*dble(t)/dble(nt))
                m = m + span
             end do
          end if
          if((patternFlag /= 0) .or. (efficiencyFlag /= 0)) then
             if(freqPoint_pat /= 1) then
                span = (freqE_pat - freqS_pat)/dble(freqPoint_pat - 1)/df
             end if
             m = freqS_pat/df
             do j=1, freqPoint_pat
                If_pat(j, i) = If_pat(j, i) &
                     & + dt*It2*exp(dcmplx(0.0d0,-2.0d0)*pi*m*dble(t)/dble(nt))
                Vf_pat(j, i) = Vf_pat(j, i) &
                     & + dt*Vt(i)*exp(dcmplx(0.0d0,-2.0d0)*pi*m*dble(t)/dble(nt))
                m = m + span
             end do
          endif
       else
       endif
    enddo
  end subroutine calc_I
  subroutine calc_Pin
    integer :: i, f
    real*8 :: Pin2(1:freqPoint_pat)
    do f=1, freqPoint_pat
       Pin(f) = 0.0d0
       do i=1, nfeed
          Pin(f) = Pin(f) + dble(If_pat(f, i)*conjg(Vf_pat(f, i)))
       end do
    end do
    
    call MPI_reduce(Pin, Pin2, freqPoint_pat, MPI_DOUBLE_PRECISION, MPI_SUM, &
         & 0, MPI_COMM_WORLD, error_code)
    if(id == 0) then
       Pin = Pin2
    end if
  end subroutine calc_Pin
  
  subroutine calc_pattern(f, theta, phi, Eth, Eph)
    integer :: f
    real*8 :: theta, phi
    real*8 :: Eth, Eph
    real*8 :: Tx,Ty,Tz, Px,Py, Rx,Ry,Rz
    real*8 :: dS, RR
    real*8 :: freq, lambda, k0
    complex*16 :: jk0, CWU
    complex*16 :: Wth, Wph, Uth, Uph
    complex*16 :: Wth2, Wph2, Uth2, Uph2
    integer :: i, j, k
    Tx=cos(theta)*cos(phi); Ty=cos(theta)*sin(phi); Tz=-sin(theta)
    Px=-sin(phi);           Py=cos(phi)
    Rx=sin(theta)*cos(phi); Ry=sin(theta)*sin(phi); Rz=cos(theta)
    
    if(freqPoint_pat /= 1) then
       freq = freqS_pat &
            & + (freqE_pat - freqS_pat)/dble(freqPoint_pat - 1)*dble(f - 1)
    else
       freq = freqS_pat
    end if
    lambda = c/freq
    k0 = 2.0d0*pi/lambda
    jk0 = dcmplx(0.0d0,k0)
    
    Wth = dcmplx(0.0d0, 0.0d0)
    Wph = Wth
    Uth = Wth
    Uph = Wth
    
    if(ZSL_pat <= ZEL_pat) then
       dS = dy*dz
       do k = ZSL_pat, ZEL_pat
          do j = YS_pat, YE_pat
             RR = dble(XS_pat)*dx*Rx &
                  & + (dble(j)+0.5d0)*dy*Ry &
                  & + (dble(k)+0.5d0)*dz*Rz
             CWU = exp(jk0*RR)*dS
             Wth = Wth - CWU*(Hy1(j,k,f)*Tz-Hz1(j,k,f)*Ty)
             Wph = Wph + CWU*Hz1(j,k,f)*Py
             Uth = Uth + CWU*(Ey1(j,k,f)*Tz-Ez1(j,k,f)*Ty)
             Uph = Uph - CWU*Ez1(j,k,f)*Py
          enddo
       enddo
       do k = ZSL_pat, ZEL_pat
          do j = YS_pat, YE_pat
             RR = dble(XE_pat+1)*dx*Rx &
                  & + (dble(j)+0.5d0)*dy*Ry &
                  & + (dble(k)+0.5d0)*dz*Rz
             CWU = exp(jk0*RR)*dS
             Wth = Wth + CWU*(Hy2(j,k,f)*Tz-Hz2(j,k,f)*Ty)
             Wph = Wph - CWU*Hz2(j,k,f)*Py
             Uth = Uth - CWU*(Ey2(j,k,f)*Tz-Ez2(j,k,f)*Ty)
             Uph = Uph + CWU*Ez2(j,k,f)*Py
          enddo
       enddo
       dS = dz*dx
       do k = ZSL_pat, ZEL_pat
          do i = XS_pat, XE_pat
             RR = (dble(i)+0.5d0)*dx*Rx &
                  & + dble(YS_pat)*dy*Ry &
                  & + (dble(k)+0.5d0)*dz*Rz
             CWU = exp(jk0*RR)*dS
             Wth = Wth - CWU*(Hz3(i,k,f)*Tx-Hx3(i,k,f)*Tz)
             Wph = Wph - CWU*Hz3(i,k,f)*Px
             Uth = Uth + CWU*(Ez3(i,k,f)*Tx-Ex3(i,k,f)*Tz)
             Uph = Uph + CWU*Ez3(i,k,f)*Px
          enddo
       enddo
       do k = ZSL_pat, ZEL_pat
          do i = XS_pat, XE_pat
             RR = (dble(i)+0.5d0)*dx*Rx &
                  & + dble(YE_pat+1)*dy*Ry &
                  & + (dble(k)+0.5d0)*dz*Rz
             CWU = exp(jk0*RR)*dS
             Wth = Wth + CWU*(Hz4(i,k,f)*Tx-Hx4(i,k,f)*Tz)
             Wph = Wph + CWU*Hz4(i,k,f)*Px
             Uth = Uth - CWU*(Ez4(i,k,f)*Tx-Ex4(i,k,f)*Tz)
             Uph = Uph - CWU*Ez4(i,k,f)*Px
          enddo
       enddo
    end if
    
    dS = dx*dy
    if((nzS(id) <= ZS_pat) .and. (ZS_pat <= nzE(id))) then
       do j = YS_pat, YE_pat
          do i = XS_pat, XE_pat
             RR = (dble(i)+0.5d0)*dx*Rx &
                  & + (dble(j)+0.5d0)*dy*Ry &
                  & + dble(ZS_pat)*dz*Rz
             CWU = exp(jk0*RR)*dS
             Wth = Wth - CWU*(Hx5(i,j,f)*Ty-Hy5(i,j,f)*Tx)
             Wph = Wph + CWU*(Hy5(i,j,f)*Px-Hx5(i,j,f)*Py)
             Uth = Uth + CWU*(Ex5(i,j,f)*Ty-Ey5(i,j,f)*Tx)
             Uph = Uph - CWU*(Ey5(i,j,f)*Px-Ex5(i,j,f)*Py)
          enddo
       enddo
    end if
    if((nzS(id) <= ZE_pat+1) .and. (ZE_pat+1 <= nzE(id))) then
       do j = YS_pat, YE_pat
          do i = XS_pat, XE_pat
             RR = (dble(i)+0.5d0)*dx*Rx &
                  & + (dble(j)+0.5d0)*dy*Ry &
                  & + dble(ZE_pat+1)*dz*Rz
             CWU = exp(jk0*RR)*dS
             Wth = Wth + CWU*(Hx6(i,j,f)*Ty-Hy6(i,j,f)*Tx)
             Wph = Wph - CWU*(Hy6(i,j,f)*Px-Hx6(i,j,f)*Py)
             Uth = Uth - CWU*(Ex6(i,j,f)*Ty-Ey6(i,j,f)*Tx)
             Uph = Uph + CWU*(Ey6(i,j,f)*Px-Ex6(i,j,f)*Py)
          enddo
       enddo
    end if
    
    RR = XC_pat*dx*Rx + YC_pat*dy*Ry + ZC_pat*dx*Rz
    CWU = jk0/(4.0d0*pi)*exp(-jk0*RR)
    Wth = CWU * Wth
    Wph = CWU * Wph
    Uth = CWU * Uth
    Uph = CWU * Uph
    
    call MPI_reduce(Wth, Wth2, 1, MPI_DOUBLE_COMPLEX, MPI_SUM, &
         & 0, MPI_COMM_WORLD, error_code)
    call MPI_reduce(Wph, Wph2, 1, MPI_DOUBLE_COMPLEX, MPI_SUM, &
         & 0, MPI_COMM_WORLD, error_code)
    call MPI_reduce(Uth, Uth2, 1, MPI_DOUBLE_COMPLEX, MPI_SUM, &
         & 0, MPI_COMM_WORLD, error_code)
    call MPI_reduce(Uph, Uph2, 1, MPI_DOUBLE_COMPLEX, MPI_SUM, &
         & 0, MPI_COMM_WORLD, error_code)
    if(id == 0) then
       Eth = abs(-Z0*Wth2-Uph2)
       Eph = abs(-Z0*Wph2+Uth2)
    end if
  end subroutine calc_pattern
  
  subroutine calc_Current_Distribution
    integer :: i,j,k,f
    real*8 :: m, span
    complex*16 :: CJ
    
    if(freqPoint_CD /= 1) then
       span = (freqE_CD - freqS_CD)/dble(freqPoint_CD-1)/df
    end if
    if((XS_CD == XE_CD).and.(YS_CD /= YE_CD) .and.(ZS_CD /= ZE_CD)) then
       i = XS_CD
       if(ZSL_CD <= ZEL_CD) then
          m = freqS_CD/df
          do f=1, freqPoint_CD
             CJ = exp(dcmplx(0.0d0, -2.0d0)*pi*m*dble(t)/dble(nt))
             do k=ZSL_CD, ZEL_CD
                do j=YS_CD, YE_CD
                   if(j .ne. YE_CD) then
                      Jy(j,k,f) = Jy(j,k,f) + CJ*Hz(i,j,k)
                      Jz(j,k,f) = Jz(j,k,f) + CJ*Hy(i,j,k)
                   end if
                end do
             end do
             m = m + span
          end do
       end if
    elseif((XS_CD /= XE_CD).and.(YS_CD == YE_CD).and.(ZS_CD /= ZE_CD)) then
       j = YS_CD
       if(ZSL_CD <= ZEL_CD) then
          m = freqS_CD/df
          do f=1, freqPoint_CD
             CJ = exp(dcmplx(0.0d0, -2.0d0)*pi*m*dble(t)/dble(nt))
             do k=ZSL_CD, ZEL_CD
                do i=XS_CD, XE_CD
                   if(i .ne. XE_CD) then
                      Jx(i,k,f) = Jx(i,k,f) + CJ*Hz(i,j,k)
                      Jz(i,k,f) = Jz(i,k,f) + CJ*Hx(i,j,k)
                   end if
                end do
             end do
             m = m + span
          end do
       end if
    elseif((XS_CD /= XE_CD).and.(YS_CD /= YE_CD).and.(ZS_CD == ZE_CD)) then
       k = ZS_CD
       if(nzS(id) <= ZS_CD .and. ZS_CD <= nzE(id)) then
          m = freqS_CD/df
          do f=1, freqPoint_CD
             CJ = exp(dcmplx(0.0d0, -2.0d0)*pi*m*dble(t)/dble(nt))
             do j=YS_CD, YE_CD
                if(j .ne. YE_CD) then
                   do i=XS_CD, XE_CD
                      if(i .ne. XE_CD) then
                         Jx(i,j,f) = Jx(i,j,f) + CJ*Hy(i,j,k)
                         Jy(i,j,f) = Jy(i,j,f) + CJ*Hx(i,j,k)
                      end if
                   end do
                end if
             end do
             m = m + span
          end do
       end if
    elseif((YS_CD == YE_CD) .and. (ZS_CD == ZE_CD)) then
       j = YS_CD
       k = ZS_CD
       if(nzS(id) <= ZS_CD .and. ZS_CD <= nzE(id)) then
          m = freqS_CD/df
          do f=1, freqPoint_CD
             CJ = exp(dcmplx(0.0d0, -2.0d0)*pi*m*dble(t)/dble(nt))
             do i = XS_CD, XE_CD
                if(i .ne. XE_CD) then
                   Jl(i,f) = Jl(i,f) + CJ*(dy*(-Hy(i,j,k) + Hy(i,j,k-1)) &
                        & + dz*(Hz(i,j,k) - Hz(i,j-1,k)))
                end if
             end do
             m = m + span
          end do
       end if
    elseif((XS_CD == XE_CD) .and. (ZS_CD == ZE_CD)) then
       i = XS_CD
       k = ZS_CD
       if(nzS(id) <= ZS_CD .and. ZS_CD <= nzE(id)) then
          m = freqS_CD/df
          do f=1, freqPoint_CD
             CJ = exp(dcmplx(0.0d0, -2.0d0)*pi*m*dble(t)/dble(nt))
             do j = YS_CD, YE_CD
                if(j .ne. YE_CD) then
                   Jl(j,f) = Jl(j,f) + CJ*(dz*(-Hz(i,j,k) + Hz(i-1,j,k)) &
                        & + dx*(Hx(i,j,k) - Hx(i,j,k-1)))
                end if
             end do
             m = m + span
          end do
       end if
    elseif((XS_CD == XE_CD) .and. (YS_CD == YE_CD)) then
       i = XS_CD
       j = YS_CD
       if(ZSL_CD <= ZEL_CD) then
          m = freqS_CD/df
          do f=1, freqPoint_CD
             CJ = exp(dcmplx(0.0d0, -2.0d0)*pi*m*dble(t)/dble(nt))
             do k=ZSL_CD, ZEL_CD
                Jl(k,f) = Jl(k,f) + CJ*(dx*(-Hx(i,j,k) + Hx(i,j-1,k)) &
                     & + dy*(Hy(i,j,k) - Hy(i-1,j,k)))
             end do
             m = m + span
          end do
       end if
    end if
  end subroutine calc_Current_Distribution
  
  subroutine calc_Electric_Field_Distribution
    integer :: i,j,k,f
    real*8 :: m, span
    complex*16 :: CE
    
    if(freqPoint_ED /= 1) then
       span = (freqE_ED - freqS_ED)/dble(freqPoint_ED-1)/df
    end if
    if(XS_ED == XE_ED) then
       i = XS_ED
       if(ZSL_ED <= ZEL_ED) then
          m = freqS_ED/df
          do f=1, freqPoint_ED
             CE = exp(dcmplx(0.0d0, -2.0d0)*pi*m*dble(t)/dble(nt))
             do k=ZSL_ED, ZEL_ED
                do j=YS_ED, YE_ED
                   if(j .ne. YE_ED) then
                      Ey_ED(j,k,f) = Ey_ED(j,k,f) + CE*Ey(i,j,k)
                      Ez_ED(j,k,f) = Ez_ED(j,k,f) + CE*Ez(i,j,k)
                   end if
                end do
             end do
             m = m + span
          end do
       end if
    else if(YS_ED == YE_ED) then
       j = YS_ED
       if(ZSL_ED <= ZEL_ED) then
          m = freqS_ED/df
          do f=1, freqPoint_ED
             CE = exp(dcmplx(0.0d0, -2.0d0)*pi*m*dble(t)/dble(nt))
             do k=ZSL_ED, ZEL_ED
                do i=XS_ED, XE_ED
                   if(i .ne. XE_ED) then
                      Ex_ED(i,k,f) = Ex_ED(i,k,f) + CE*Ex(i,j,k)
                      Ez_ED(i,k,f) = Ez_ED(i,k,f) + CE*Ez(i,j,k)
                   end if
                end do
             end do
             m = m + span
          end do
       end if
    else if(ZS_ED == ZE_ED) then
       k = ZS_ED
       if(nzS(id) <= ZS_ED .and. ZS_ED <= nzE(id)) then
          m = freqS_ED/df
          do f=1, freqPoint_ED
             CE = exp(dcmplx(0.0d0, -2.0d0)*pi*m*dble(t)/dble(nt))
             do j=YS_ED, YE_ED
                if(j .ne. YE_ED) then
                   do i=XS_ED, XE_ED
                      if(i .ne. XE_ED) then
                         Ex_ED(i,j,f) = Ex_ED(i,j,f) + CE*Ex(i,j,k)
                         Ey_ED(i,j,f) = Ey_ED(i,j,f) + CE*Ey(i,j,k)
                      end if
                   end do
                end if
             end do
             m = m + span
          end do
       end if
    end if
  end subroutine calc_Electric_Field_Distribution
  
  real*8 function gauss_pulse(x, t0, alpha, amp)
    real*8 :: x, t0, alpha, amp
    if((x >= 0.0d0).and.(x <= 2.0d0*t0)) then
       gauss_pulse = exp(-alpha*((x-t0)*(x-t0)))*amp
    else
       gauss_pulse = 0.0d0
    endif
  end function gauss_pulse
  real*8 function d_gauss_pulse(x, t0, alpha, amp)
    real*8 :: x, t0, alpha, amp
    if((x >= 0.0d0).and.(x <= 2.0d0*t0)) then
       d_gauss_pulse = (x-t0)*exp(-alpha*((x-t0)*(x-t0)))*amp/t0
    else
       d_gauss_pulse = 0.0d0
    endif
  end function d_gauss_pulse
  real*8 function sin_pulse(x, t0, amp)
    real*8 :: x, t0, amp
    sin_pulse = amp*sin(2.0d0*pi*x/t0)
  end function sin_pulse
end module fdtd
module output
  use setup
  use fdtd
  implicit none
contains
  subroutine out_impedance
    integer :: i,j
    real*8 :: f,span
    complex*16 :: ref
    
    if(id == 0) then
       call progress_bar('Impedance calculation', 0, freqPoint_imp, 0, 0, 20)
       open(150, file = trim(filename)//'_imp.txt',status='replace')
       close(150)
       open(151, file = trim(filename)//'_VSWR.txt',status='replace')
       close(151)
    end if
    
    f = freqS_imp
    if(freqPoint_imp /= 1) then
       span = (freqE_imp - freqS_imp)/dble(freqPoint_imp - 1)
    else
       span = 0.0d0
    end if
    
    do j=1, freqPoint_imp
       if(id == 0) then
          open(150, file=trim(filename)//'_imp.txt', &
               & status='old', position='append')
          write(150,'(G14.4E3,$)') f
          close(150)
          open(151, file=trim(filename)//'_VSWR.txt', &
               & status='old', position='append')
          write(151,'(G14.4E3,$)') f
          close(151)
       end if
       call MPI_barrier(MPI_COMM_WORLD, error_code)
       do i=1, nfeed
          if((nzS(id) <= feedcz(i)) .and. (feedcz(i) <= nzE(id))) then
             imp(j,i) = Vf_imp(j,i)/If_imp(j,i)
             open(150, file=trim(filename)//'_imp.txt', &
                  & status='old', position='append')
             write(150,'(2G14.4E3,$)') dble(imp(j,i)),aimag(imp(j,i))
             close(150)
             ref = (imp(j,i)-ZL)/(imp(j,i)+ZL)
             open(151, file=trim(filename)//'_VSWR.txt', &
                  & status='old', position='append')
             write(151,'(G14.4E3,$)') (1.0d0+abs(ref))/(1.0d0-abs(ref))
             close(151)
          end if
          call MPI_barrier(MPI_COMM_WORLD, error_code)
       end do
       if(id == 0) then
          open(150, file=trim(filename)//'_imp.txt', &
               & status='old',position='append')
          write(150,*)
          close(150)
          open(151, file=trim(filename)//'_VSWR.txt', &
               & status='old',position='append')
          write(151,*)
          close(151)
       end if
       f = f + span
       if(id == 0) then
          call progress_bar('Impedance calculation', j, freqPoint_imp, 0, 0, 20)
       end if
    end do
  end subroutine out_impedance
  subroutine out_VI
    integer :: i,j
    real*8 :: f,span
    if(id == 0) then
       call progress_bar('VI output', 0, freqPoint_imp, 0, 0, 20)
       open(150, file = trim(filename)//'_VI.txt',status='replace')
       close(150)
    end if
    f = freqS_imp
    if(freqPoint_imp /= 1) then
       span = (freqE_imp - freqS_imp)/dble(freqPoint_imp - 1)
    else
       span = 0.0d0
    end if
    do j=1, freqPoint_imp
       if(id == 0) then
          open(150, file=trim(filename)//'_VI.txt', &
               & status='old', position='append')
          write(150,'(G14.4E3,$)') f
          close(150)
       end if
       call MPI_barrier(MPI_COMM_WORLD, error_code)
       do i=1, nfeed
          if((nzS(id) <= feedcz(i)) .and. (feedcz(i) <= nzE(id))) then
             open(150, file=trim(filename)//'_VI.txt', &
                  & status='old', position='append')
             write(150,'(4G14.4E3,$)') Vf_imp(j,i),If_imp(j,i)
             close(150)
          end if
          call MPI_barrier(MPI_COMM_WORLD, error_code)
       end do
       if(id == 0) then
          open(150, file=trim(filename)//'_VI.txt', &
               & status='old', position='append')
          write(150,*)
          close(150)
       end if
       f = f + span
       if(id == 0) then
          call progress_bar('VI output', j, freqPoint_imp, 0, 0, 20)
       end if
    end do
    call MPI_barrier(MPI_COMM_WORLD, error_code) 
  end subroutine out_VI
  subroutine out_pattern
    real*8 :: Eth(1:3), Eph(1:3)
    integer :: f, L
    if(id == 0) then
       call progress_bar('Pattern calculation', 0, freqPoint_pat, 0, 0, 20)
       open(150,file=trim(filename)//'_pat.txt')
       write(150, '(6E12.4)') freqS_pat, freqE_pat, dble(freqPoint_pat), &
            & XC_pat, YC_pat, ZC_pat
    end if
    do f=1, freqPoint_pat
       do L=0, nAngle
          call calc_pattern(f, pi/2.0d0, dble(L)/dble(nAngle)*2.0d0*pi, &
               & Eth(1), Eph(1))
          call calc_pattern(f, -dble(L)/dble(nAngle)*2.0d0*pi+pi/2.0d0, &
               & pi/2.0d0, Eth(2), Eph(2))
          call calc_pattern(f, dble(L)/dble(nAngle)*2.0d0*pi, 0.0d0, &
               & Eth(3), Eph(3))
          if(id == 0) then
             write(150,'(6E12.4)') Eth(1)*Eth(1)/Pin(f)/30.0d0, &
                  & Eph(1)*Eph(1)/Pin(f)/30.0d0, &
                  & Eth(2)*Eth(2)/Pin(f)/30.0d0, &
                  & Eph(2)*Eph(2)/Pin(f)/30.0d0, &
                  & Eth(3)*Eth(3)/Pin(f)/30.0d0, &
                  & Eph(3)*Eph(3)/Pin(f)/30.0d0
             call progress_bar('Pattern calculation', f, freqPoint_pat, &
                  & L+1, nAngle+1, 20)
          end if
       end do
    end do
    if(id == 0) then
       close(150)
    end if
  end subroutine out_pattern
  subroutine calc_Prad
    integer :: f
    real*8 :: dS
    real*8 :: Prad2(1:freqPoint_pat)
    
    if(id == 0) then
       call progress_bar('Efficiency calculation', 0, freqPoint_pat, 0, 0, 20)
    end if
    Prad = 0.0d0
    do f=1, freqPoint_pat
       if(ZSL_pat <= ZEL_pat) then
          dS = dy*dz
          Prad(f) = Prad(f) - sum((dble((Ey1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & * conjg(Hz1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f)) &
               & - Ez1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & * conjg(Hy1(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f)))))*dS)
          Prad(f) = Prad(f) + sum((dble((Ey2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & * conjg(Hz2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f)) &
               & - Ez2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f) &
               & * conjg(Hy2(YS_pat:YE_pat,ZSL_pat:ZEL_pat,f)))))*dS)
          dS = dz*dx
          Prad(f) = Prad(f) - sum((dble((Ez3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & * conjg(Hx3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f)) &
               & - Ex3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & * conjg(Hz3(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f)))))*dS)
          Prad(f) = Prad(f) + sum((dble((Ez4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & * conjg(Hx4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f)) &
               & - Ex4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f) &
               & * conjg(Hz4(XS_pat:XE_pat,ZSL_pat:ZEL_pat,f)))))*dS)
          dS = dx*dy
       end if
       if((nzS(id) <= ZS_pat) .and. (ZS_pat <= nzE(id))) then
          Prad(f) = Prad(f) - sum((dble((Ex5(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & * conjg(Hy5(XS_pat:XE_pat,YS_pat:YE_pat,f)) &
               & - Ey5(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & * conjg(Hx5(XS_pat:XE_pat,YS_pat:YE_pat,f)))))*dS)
       end if
       if((nzS(id) <= ZE_pat+1) .and. (ZE_pat+1 <= nzE(id))) then
          Prad(f) = Prad(f) + sum((dble((Ex6(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & * conjg(Hy6(XS_pat:XE_pat,YS_pat:YE_pat,f)) &
               & - Ey6(XS_pat:XE_pat,YS_pat:YE_pat,f) &
               & * conjg(Hx6(XS_pat:XE_pat,YS_pat:YE_pat,f)))))*dS)
       end if
       if(id == 0) then
          call progress_bar('Efficiency calculation', f, freqPoint_pat, 0, 0, 20)
       end if
    end do
    
    call MPI_reduce(Prad, Prad2, freqPoint_pat, MPI_DOUBLE_PRECISION, MPI_SUM, &
         & 0, MPI_COMM_WORLD, error_code)
    if(id == 0) then
       Prad = Prad2
    end if
  end subroutine calc_Prad
 
  subroutine out_efficiency
    integer :: f
    real*8 :: freq
    if(id == 0) then
       open(150,file=trim(filename)//'_eff.txt')
       do f=1, freqPoint_pat
          if(freqPoint_pat /= 1) then
             freq = freqS_pat &
                  & + (freqE_pat - freqS_pat)/dble(freqPoint_pat - 1)*dble(f - 1)
          else
             freq = freqS_pat
          end if
          write(150,'(2E12.4)') freq, Prad(f)/Pin(f)*100.0d0
       end do
       close(150)
    end if
  end subroutine out_efficiency
  
  subroutine out_Current_Distribution
    integer :: i,j,k,f,p
    
    if(id == 0) then
       if (XS_CD /= XE_CD) then
          open(150, file = trim(filename)//'_Jx.txt',status='replace')
          write(150,'(9E12.4)') dble(XS_CD), dble(YS_CD), dble(ZS_CD), &
               & dble(XE_CD), dble(YE_CD), dble(ZE_CD), &
               & freqS_CD, freqE_CD, dble(freqPoint_CD)
          close(150)
       end if
       if (YS_CD /= YE_CD) then
          open(150, file = trim(filename)//'_Jy.txt',status='replace')
          write(150,'(9E12.4)') dble(XS_CD), dble(YS_CD), dble(ZS_CD), &
               & dble(XE_CD), dble(YE_CD), dble(ZE_CD), &
               & freqS_CD, freqE_CD, dble(freqPoint_CD)
          close(150)
       end if
       if (ZS_CD /= ZE_CD) then
          open(150, file = trim(filename)//'_Jz.txt',status='replace')
          write(150,'(9E12.4)') dble(XS_CD), dble(YS_CD), dble(ZS_CD), &
               & dble(XE_CD), dble(YE_CD), dble(ZE_CD), &
               & freqS_CD, freqE_CD, dble(freqPoint_CD)
          close(150)
       end if
    end if
    call MPI_barrier(MPI_COMM_WORLD, error_code)
    if((XS_CD == XE_CD).and.(YS_CD /= YE_CD).and.(ZS_CD /= ZE_CD)) then
       do f=1, freqPoint_CD
          do p=0, np-1
             if(p == id) then
                if(ZSL_CD <= ZEL_CD) then
                   open(150, file=trim(filename)//'_Jy.txt', &
                        & status='old',position='append')
                   open(151, file=trim(filename)//'_Jz.txt', &
                        & status='old',position='append')
                   do k=ZSL_CD, ZEL_CD
                      do j=YS_CD, YE_CD
                         if(j .ne. YE_CD) then
                            write(150, '(1E12.4,$)') abs(Jy(j,k,f))
                            write(151, '(1E12.4,$)') abs(Jz(j,k,f))
                         end if
                      end do
                      write(150,*) ''
                      write(151,*) ''
                   end do
                   close(150)
                   close(151)
                end if
             end if
             call MPI_barrier(MPI_COMM_WORLD, error_code)
          end do
       end do
    elseif((XS_CD /= XE_CD).and.(YS_CD == YE_CD).and.(ZS_CD /= ZE_CD)) then
       do f=1, freqPoint_CD
          do p=0, np-1
             if(p == id) then
                if(ZSL_CD <= ZEL_CD) then
                   open(150, file=trim(filename)//'_Jx.txt', &
                        & status='old',position='append')
                   open(151, file=trim(filename)//'_Jz.txt', &
                        & status='old',position='append')
                   do k=ZSL_CD, ZEL_CD
                      do i=XS_CD, XE_CD
                         if (i .ne. XE_CD) then
                            write(150, '(1E12.4,$)') abs(Jx(i,k,f))
                            write(151, '(1E12.4,$)') abs(Jz(i,k,f))
                         end if
                      end do
                      write(150,*) ''
                      write(151,*) ''
                   end do
                   close(150)
                   close(151)
                end if
             end if
             call MPI_barrier(MPI_COMM_WORLD, error_code)
          end do
       end do
    elseif((XS_CD /= XE_CD).and.(YS_CD /= YE_CD).and.(ZS_CD == ZE_CD)) then
       if(nzS(id) <= ZS_CD .and. ZS_CD <= nzE(id)) then
          open(150, file=trim(filename)//'_Jx.txt', &
               & status='old',position='append')
          open(151, file=trim(filename)//'_Jy.txt', &
               & status='old',position='append')
          do f=1, freqPoint_CD
             do j=YS_CD, YE_CD
                if(j .ne. YE_CD) then
                   do i=XS_CD, XE_CD
                      if(i .ne. XE_CD) then
                         write(150, '(1E12.4,$)') abs(Jx(i,j,f))
                         write(151, '(1E12.4,$)') abs(Jy(i,j,f))
                      end if
                   end do
                   write(150,*) ''
                   write(151,*) ''
                end if
             end do
          end do
          close(150)
          close(151)
       end if
    elseif((YS_CD == YE_CD).and.(ZS_CD == ZE_CD)) then
       if(nzS(id) <= ZS_CD .and. ZS_CD <= nzE(id)) then
          open(150, file=trim(filename)//'_Jx.txt', &
               & status='old',position='append')
          do f=1, freqPoint_CD
             do i=XS_CD, XE_CD
                if(i .ne. XE_CD) then
                   write(150, '(1E12.4,$)') abs(Jl(i,f))
                end if
             end do
             write(150,*) ''
          end do
          close(150)
       end if
    elseif((XS_CD == XE_CD).and.(ZS_CD == ZE_CD)) then
       if(nzS(id) <= ZS_CD .and. ZS_CD <= nzE(id)) then
          open(150, file=trim(filename)//'_Jy.txt', &
               & status='old',position='append')
          do f=1, freqPoint_CD
             do j=YS_CD, YE_CD
                if(j .ne. YE_CD) then
                   write(150, '(1E12.4,$)') abs(Jl(j,f))
                end if
             end do
             write(150,*) ''
          end do
          close(150)
       end if
    elseif((XS_CD == XE_CD).and.(YS_CD == YE_CD)) then
       do f=1, freqPoint_CD
          do p=0, np-1
             if(p == id) then
                if(ZSL_CD <= ZEL_CD) then
                   open(150, file=trim(filename)//'_Jz.txt', &
                        & status='old',position='append')
                   do k=ZSL_CD, ZEL_CD
                      write(150, '(1E12.4,$)') abs(Jl(k,f))
                   end do
                   close(150)
                end if
             end if
             call MPI_barrier(MPI_COMM_WORLD, error_code)
          end do
          if(id == 0) then
             open(150, file=trim(filename)//'_Jz.txt', &
                  & status='old',position='append')
             write(*,*) ''
             close(150)
          end if
          call MPI_barrier(MPI_COMM_WORLD, error_code)
       end do
    end if
    call MPI_barrier(MPI_COMM_WORLD, error_code)
  end subroutine out_Current_Distribution
  
  subroutine out_Electric_Field_Distribution
    integer :: i,j,k,f,p
    
    if(id == 0) then
       if (XS_ED /= XE_ED) then
          open(150, file = trim(filename)//'_Ex.txt',status='replace')
          write(150,'(9E12.4)') dble(XS_ED), dble(YS_ED), dble(ZS_ED), &
               & dble(XE_ED), dble(YE_ED), dble(ZE_ED), &
               & freqS_ED, freqE_ED, dble(freqPoint_ED)
          close(150)
       end if
       if (YS_ED /= YE_ED) then
          open(150, file = trim(filename)//'_Ey.txt',status='replace')
          write(150,'(9E12.4)') dble(XS_ED), dble(YS_ED), dble(ZS_ED), &
               & dble(XE_ED), dble(YE_ED), dble(ZE_ED), &
               & freqS_ED, freqE_ED, dble(freqPoint_ED)
          close(150)
       end if
       if (ZS_ED /= ZE_ED) then
          open(150, file = trim(filename)//'_Ez.txt',status='replace')
          write(150,'(9E12.4)') dble(XS_ED), dble(YS_ED), dble(ZS_ED), &
               & dble(XE_ED), dble(YE_ED), dble(ZE_ED), &
               & freqS_ED, freqE_ED, dble(freqPoint_ED)
          close(150)
       end if
    end if
    call MPI_barrier(MPI_COMM_WORLD, error_code)
    if(XS_ED == XE_ED) then
       do f=1, freqPoint_ED
          do p=0, np-1
             if(p == id) then
                if(ZSL_ED <= ZEL_ED) then
                   open(150, file=trim(filename)//'_Ey.txt', &
                        & status='old',position='append')
                   open(151, file=trim(filename)//'_Ez.txt', &
                        & status='old',position='append')
                   do k=ZSL_ED, ZEL_ED
                      do j=YS_ED, YE_ED
                         if(j .ne. YE_ED) then
                            write(150, '(1E12.4,$)') abs(Ey_ED(j,k,f))
                            write(151, '(1E12.4,$)') abs(Ez_ED(j,k,f))
                         end if
                      end do
                      write(150,*) ''
                      write(151,*) ''
                   end do
                   close(150)
                   close(151)
                end if
             end if
             call MPI_barrier(MPI_COMM_WORLD, error_code)
          end do
       end do
    else if(YS_ED == YE_ED) then
       do f=1, freqPoint_ED
          do p=0, np-1
             if(p == id) then
                if(ZSL_ED <= ZEL_ED) then
                   open(150, file=trim(filename)//'_Ex.txt', &
                        & status='old',position='append')
                   open(151, file=trim(filename)//'_Ez.txt', &
                        & status='old',position='append')
                   do k=ZSL_ED, ZEL_ED
                      do i=XS_ED, XE_ED
                         if (i .ne. XE_ED) then
                            write(150, '(1E12.4,$)') abs(Ex_ED(i,k,f))
                            write(151, '(1E12.4,$)') abs(Ez_ED(i,k,f))
                         end if
                      end do
                      write(150,*) ''
                      write(151,*) ''
                   end do
                   close(150)
                   close(151)
                end if
             end if
             call MPI_barrier(MPI_COMM_WORLD, error_code)
          end do
       end do
    else if(ZS_ED == ZE_ED) then
       if(nzS(id) <= ZS_ED .and. ZS_ED <= nzE(id)) then
          open(150, file=trim(filename)//'_Ex.txt', &
               & status='old',position='append')
          open(151, file=trim(filename)//'_Ey.txt', &
               & status='old',position='append')
          do f=1, freqPoint_ED
             do j=YS_ED, YE_ED
                if(j .ne. YE_ED) then
                   do i=XS_ED, XE_ED
                      if(i .ne. XE_ED) then
                         write(150, '(1E12.4,$)') abs(Ex_ED(i,j,f))
                         write(151, '(1E12.4,$)') abs(Ey_ED(i,j,f))
                      end if
                   end do
                   write(150,*) ''
                   write(151,*) ''
                end if
             end do
          end do
          close(150)
          close(151)
       end if
    end if
    call MPI_barrier(MPI_COMM_WORLD, error_code)
  end subroutine out_Electric_Field_Distribution
  
  subroutine progress_bar(title, n, max, n_sub, max_sub, bar_length)
    character(*) :: title
    integer :: n, max, n_sub, max_sub, bar_length
    character :: BS!, DEL
    character*128 :: tempstr, tempstr2
    integer, save :: starttime(8)
    integer :: now(8)
    real*8 :: elapsed_time
    integer :: i
    
    if(n == 0) then
       call date_and_time(values=starttime)
    end if
    
    BS = char(8)
    
    do i=1, 256
       write(*, '(1A$)') BS
    end do
    write(*, '(1A$)') trim(title)
    write(*, '(1A$)') ' ['
    do i=0, bar_length*n/max
       if(i /= 0) then
          write(*, '(1A$)') '*'
       end if
    end do
    do i=bar_length*n/max, bar_length
       if (i /= bar_length) then
          write(*, '(1A$)')' '
       end if
    end do
    write(tempstr, *) n
    write(tempstr2, *) max
    tempstr = '] (' // trim(tempstr) // '/' // trim(tempstr2) // ')'
    write(*, '(1A$)') trim(tempstr)
    
    call date_and_time(values=now)
    elapsed_time = dble((now(3) - starttime(3))*starttime(4)) &
         & *24.0d0*60.0d0*60.0d0*1.0d3 &
         & + dble(now(4) - starttime(4))*24.0d0*60.0d0*60.0d0*1.0d3 &
         & + dble(now(5) - starttime(5))*60.0d0*60.0d0*1.0d3 &
         & + dble(now(6) - starttime(6))*60.0d0*1.0d3 &
         & + dble(now(7) - starttime(7))*1.0d3 &
         & + dble(now(8) - starttime(8))
    if(max_sub == 0) then
       write(tempstr, *) nint(elapsed_time/1.0d3/dble(n)*dble(max-n))
    else
       write(tempstr, *) nint(elapsed_time/1.0d3/dble((n-1)*max_sub+n_sub) &
            & * dble((max-n)*max_sub+(max_sub-n_sub)))
    end if
    tempstr =  ' Estimate time :' // trim(tempstr) // '(sec)'
    write(*, '(1A$)') trim(tempstr)
    
    if((n == max) .and. (n_sub == max_sub)) then
       write(*,*) ''
    end if
    
    call flush(6)
  end subroutine progress_bar
end module output
program main
  use setup
  use fdtd
  use output
  implicit none
  
  integer :: z,i
  
  call beforeInit
  call initialize
  call setmodel
  call SetMaterialAtCPcell
  call SetMaterialAtFeedPoint
  
  if(id == 0) then
     call progress_bar('E-H calculation', 0, nt, 0, 0, 20)
  end if
  do t=0, nt-1
     ! $(B5kEE(B
     call calc_feed
     ! $(BAw?.LLEE3&7W;;(B
     if(PLRCFlag /= 0) then
        call calc_PHI(nzS(id))
     end if
     call calc_E(nzS(id))
     if(PLRCFlag /= 0) then
        call E_PHI_update(nzS(id))
     end if
     ! $(BHsF14|Aw<u?.(B
     call MPI_Isend(Ex(1,1,nzS(id)), 1, MatrixXY, &
          & zm, 0, MPI_COMM_WORLD, request(1), error_code)
     call MPI_Isend(Ey(1,1,nzS(id)), 1, MatrixXY, &
          & zm, 1, MPI_COMM_WORLD, request(2), error_code)
     call MPI_Irecv(Ex(1,1,nzE(id)+1), 1, MatrixXY, &
          & zp, 0, MPI_COMM_WORLD, request(3), error_code)
     call MPI_Irecv(Ey(1,1,nzE(id)+1), 1, MatrixXY, &
          & zp, 1, MPI_COMM_WORLD, request(4), error_code)
     ! $(BEE3&7W;;(B
     do z=nzS(id)+1, nzE(id)
        if(PLRCFlag /= 0) then
           call calc_PHI(z)
        end if
        call calc_E(z)
        if(PLRCFlag /= 0) then
           call E_PHI_update(z)
        end if
     enddo
     call MPI_Waitall(2, request(3:4), status(:,3:4), error_code)
     if((patternFlag /= 0) .or. (efficiencyFlag /= 0)) then
     call calc_surface_Jm
     endif
     if(EDFlag /= 0) then
        call calc_Electric_Field_Distribution
     end if
     call MPI_Waitall(2, request(1:2), status(:,1:2), error_code)
     call calc_H(nzE(id))
     if((CPFlag == 1) .and. (cpPoint > 0) .and. (cpS(nzE(id)) /= -1)) then
        call calc_H_CP(nzE(id))
     end if
     call MPI_Isend(Hx(1,1,nzE(id)), 1, MatrixXY, &
          & zp, 0, MPI_COMM_WORLD, request(1), error_code)
     call MPI_Isend(Hy(1,1,nzE(id)), 1, MatrixXY, &
          & zp, 1, MPI_COMM_WORLD, request(2), error_code)
     call MPI_Irecv(Hx(1,1,nzS(id)-1), 1, MatrixXY, &
          & zm, 0, MPI_COMM_WORLD, request(3), error_code)
     call MPI_Irecv(Hy(1,1,nzS(id)-1), 1, MatrixXY, &
          & zm, 1, MPI_COMM_WORLD, request(4), error_code) 
     do z=nzE(id)-1, nzS(id), -1
        call calc_H(z)
        if(CPFlag == 1 .and. cpPoint > 0 .and. cpS(z) /= -1) then
           call calc_H_CP(z)
        end if
     end do
     call MPI_Waitall(2, request(3:4), status(:,3:4), error_code)
     call calc_I
     if((patternFlag /= 0) .or. (efficiencyFlag /= 0)) then
     call calc_surface_J
     end if
     if(CDFlag /= 0) then
        call calc_Current_Distribution
     end if
     call MPI_Waitall(2, request(1:2), status(:,1:2), error_code)
     do i=1, nfeed
        if((feedAxis(i) == 'z') .and. (feedLength(i) /= 1)) then
           call MPI_Bcast(It(i), 1, MPI_DOUBLE_PRECISION, &
                & feedNode(i), MPI_COMM_WORLD, error_code)
        end if
     end do
     if(id == 0) then
        call progress_bar('E-H calculation', t+1, nt, 0, 0, 20)
     end if
  end do
  
  ! $(B3F<o=PNO(B
  if(impedanceFlag /= 0) then
     call out_impedance
     call out_VI
  end if
  if((patternFlag /= 0) .or. (efficiencyFlag /= 0)) then
     call calc_Pin
  end if
  if(patternFlag /= 0) then
     call out_pattern
  end if
  if(efficiencyFlag /= 0) then
     call calc_Prad
     call out_efficiency
  end if
  if(CDFlag /= 0) then
     call out_Current_Distribution
  end if
  if(EDFlag /= 0) then
     call out_Electric_Field_Distribution
  end if
  
  ! $(B=*N;=hM}(B
  call MPI_Finalize(error_code)
end program main
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!         !!!!!!              !!!!!!!!!       !!!!!!
!!  !!!!!!  !!!!!!!!  !!!!  !!!!!!!!!   !!!!!!!  !!!!
!!  !!!!!!  !!!!!!!!  !!!!  !!!!!!!!  !!!!!!!!!!  !!!
!!  !!!!!  !!!!!!!!!  !!!!  !!!!!!!  !!!!!!!!!!!!  !!
!!  !!   !!!!!!!!!!!  !!!!  !!!!!!!  !!!!!!!!!!!!  !!
!!      !!!!!!!!!!!!  !!!!  !!!!!!!  !!!!!!!!!!!!  !!
!!  !!!!  !!!!!!!!!!  !!!!  !!!!!!!  !!!!!!!!!!!!  !!
!!  !!!!!  !!!!!!!!!  !!!!  !!!!!!!!  !!!!!!!!!!  !!!
!!  !!!!!!   !!!!!!!  !!!!  !!!!!!!!!   !!!!!!!  !!!!
!!  !!!!!!!   !!!              !!!!!!!!!       !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!! 2008.Oct. Maeda Tdahiko Laboratory !!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

