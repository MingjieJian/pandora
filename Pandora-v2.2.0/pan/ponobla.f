      subroutine PONOBLA
     $(LU,N,PE,FE,EP,BS,GTN,S)
C
C     Rudolf Loeser, 2005 Apr 05
C---- Prints line-related data used as input for the PRD calculations.
C     (This is version 2 of PONOBLA.)
C     !DASH
      save
C     !DASH
      real*8 BS, EP, FE, GTN, PE, S
      integer I, IPRDD, LU, N
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 98),IPRDD)
C     !DASH
      external LINER, HI, BYE
C
C               PE(N), FE(N), EP(N), BS(N), GTN(N), S(N)
      dimension PE(*), FE(*), EP(*), BS(*), GTN(*), S(*)
C
      call HI ('PONOBLA')
C     !BEG
      if(LU.gt.0) then
        call LINER (2, LU)
        write (LU,100)
  100   format(' ',15X,'PE',11X,'FE',11X,'EP',11X,'BS',10X,'GTN',
     $             12X,'S')
        call LINER (1, LU)
        write (LU,101) (I,PE(I),FE(I),EP(I),BS(I),GTN(I),S(I),
     $                  I=1,N,IPRDD)
  101   format(5(' ',I4,1P6E13.5/))
      end if
C     !END
      call BYE ('PONOBLA')
C
      return
      end
