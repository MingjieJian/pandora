      subroutine LUGG
     $(KK,N,DLC,OMD,FNDT,S,GA,CP,XJBAR)
C
C     Rudolf Loeser, 2004 May 06
C---- Dumps for BRASS.
C     !DASH
      save
C     !DASH
      real*8 CP, DLC, FNDT, GA, OMD, S, XJBAR
      integer I, KK, LUEO, N
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
C               DLC(N), OMD(N), FNDT(N), S(N), GA(N), CP(N), XJBAR(N)
      dimension DLC(*), OMD(*), FNDT(*), S(*), GA(*), CP(*), XJBAR(*)
C
      call HI ('LUGG')
C     !BEG
      call LINER  (1, LUEO)
      write (LUEO,100)
  100 format(' ',17X,'DLC',12X,'OMD',11X,'FNDT',14X,'S',13X,'GA',
     $           13X,'CP',11X,'JBAR')
      call LINER  (1, LUEO)
      write (LUEO,101) (I,DLC(I),OMD(I),FNDT(I),S(I),GA(I),CP(I),
     $                  XJBAR(I),I=KK,N)
  101 format(5(' ',I5,1P7E15.7/))
C     !END
      call BYE ('LUGG')
C
      return
      end
