      subroutine WELL
     $(N,S,XJ,KODE)
C
C     Rudolf Loeser, 1978 Apr 09
C---- Dumps, for SASKIA.
C     !DASH
      save
C     !DASH
      real*8 S, XJ
      integer I, KODE, LUEO, N
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
C               S(N), XJ(N)
      dimension S(*), XJ(*)
C
      call HI ('WELL')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) KODE
  100 format(' ','KODE =',I5)
      call LINER (1, LUEO)
C
      write (LUEO,101) (I,S(I),XJ(I),I=1,N)
  101 format(' ',23X,'S',19X,'J'//
     $     5(' ',I4,1P2E20.10/))
C     !END
      call BYE ('WELL')
C
      return
      end
