      subroutine BASALT
     $(KILROY,IU,IL,N,Z,K,DL,XJNU)
C
C     Rudolf Loeser, 1980 Jun 06
C---- Saves XJNU in file LU.
C     (This is version 3 of BASALT.)
C     !DASH
      save
C     !DASH
      real*8 DL, XJNU, Z
      integer IL, IU, JAYTO, K, N
      logical KILROY
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(18),JAYTO)
C     !DASH
      external RUFF, CABBAGE, HI, BYE
C
C               Z(N), DL(K), XJNU(N,K)
      dimension Z(*), DL(*), XJNU(*)
C
      call HI ('BASALT')
C     !BEG
      if(KILROY) then
C----   Initialize file
        call RUFF  (JAYTO)
      end if
C
C---- Write the current data set, i.e. for transition IU/IL
      write (JAYTO,100) N,K
  100 format(2I4)
C
      call CABBAGE (JAYTO, 1, N, K, IU, IL, Z, DL, XJNU)
C     !END
      call BYE ('BASALT')
C
      return
      end
