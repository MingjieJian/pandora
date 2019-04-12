      subroutine HOOPOE
     $(N,NL,IHEDF,XNK,XND,H1,HK,HE11,HE1K,HE21,HE2K,OK)
C
C     Rudolf Loeser, 2004 May 21
C---- Validates number density arrays before beginning the
C     diffusion calculations.
C     !DASH
      save
C     !DASH
      real*8 H1, HE11, HE1K, HE21, HE2K, HK, XND, XNK
      integer IHEDF, J, LUEO, N, NL
      logical OK
      character LAB*4
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external OUZEL, MASHED, HI, BYE
C
C               XND(N,NL), HE11(N), HE1K(N), HE21(N), HE2K(N), XNK(N),
      dimension XND(N,*),  HE11(*), HE1K(*), HE21(*), HE2K(*), XNK(*),
C
C               H1(N), HK(N)
     $          H1(*), HK(*)
C
      call HI ('HOOPOE')
C     !BEG
      OK = .true.
C
      do 101 J = 1,NL
        write (LAB,100) J
  100   format('ND',I2)
        call OUZEL (N, XND(1,J), LAB,    OK, 'HOOPOE')
  101 continue
      call OUZEL   (N, XNK,      'NK',   OK, 'HOOPOE')
C
      call OUZEL   (N, H1,       'H1',   OK, 'HOOPOE')
      call OUZEL   (N, HK,       'HK',   OK, 'HOOPOE')
C
      if(IHEDF.le.0) then
        call OUZEL (N, HE11,     'HE1',  OK, 'HOOPOE')
        call OUZEL (N, HE1K,     'HEK',  OK, 'HOOPOE')
        call OUZEL (N, HE21,     'HE21', OK, 'HOOPOE')
        call OUZEL (N, HE2K,     'HE2K', OK, 'HOOPOE')
      end if
C
      if(.not.OK) then
        write (LUEO,102)
  102   format(' ','********** No diffusion calculations can be done.')
        call MASHED ('HOOPOE')
      end if
C     !END
      call BYE ('HOOPOE')
C
      return
      end
