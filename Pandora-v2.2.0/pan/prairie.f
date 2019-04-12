      subroutine PRAIRIE
     $(X,W,G,H,IMG,HOK)
C
C     Rudolf Loeser, 1980 Sep 24
C---- Computes H, for H.S.E.
C     Returns with HOK = .true. if H seems ok, = .false. if not.
C     (This is version 2 of PRAIRIE.)
C     !DASH
      save
C     !DASH
      real*8 G, H, HLIM, W, X
      integer I, IMG, LUEO, N
      logical HOK
      character LABEL*100
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external FELIPE, MESHED, MASHED, HI, BYE
C
      dimension X(*), W(*)
C
C               H(N), G(N), IMG(N)
      dimension H(*), G(*), IMG(*)
C
      data LABEL /'H, for H.S.E.'/
      data HLIM  /5.D2/
C
      call HI ('PRAIRIE')
C     !BEG
      call FELIPE     (X, W, N, G, H, LABEL, IMG)
C
      HOK = .true.
      do 101 I = 1,N
        if(H(I).gt.HLIM) then
C
          call MESHED ('PRAIRIE', 2)
          write (LUEO,100) HLIM,I
  100     format(' ','HLIM =',1PE10.2,'; H exceeds HLIM at depth',I6)
          call MASHED ('PRAIRIE')
C
          HOK = .false.
          goto 102
        end if
  101 continue
C
  102 continue
C     !END
      call BYE ('PRAIRIE')
C
      return
      end
