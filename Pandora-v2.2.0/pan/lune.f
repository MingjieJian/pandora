      subroutine LUNE
     $(N,PQ,CP,S,RHO)
C
C     Rudolf Loeser, 2004 May 06
C---- Dumps for BRONZE.
C     (This is version 2 of LUNE.)
C     !DASH
      save
C     !DASH
      real*8 CP, PQ, RHO, S
      integer I, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
C               PQ(N), CP(N), S(N), RHO(N)
      dimension PQ(*), CP(*), S(*), RHO(*)
C
      call HI ('LUNE')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100)
  100 format(' ',18X,'PQ',13X,'CP',14X,'S',12X,'RHO')
      call LINER (1, LUEO)
      write (LUEO,101) (I,PQ(I),CP(I),S(I),RHO(I),I=1,N)
  101 format(5(' ',I5,1P4E15.7/))
C     !END
      call BYE ('LUNE')
C
      return
      end
