      subroutine ERENI
     $(LUIS,N,IU,IL,CORE,IOVER,ISUB,WPOP,WBD,JRHO,JBD,METSE,METEP,WEP,
     $ Z,TE,EP,TAU,S,RHO,RHOWT,XJBR)
C
C     Rudolf Loeser, 1987 Jan 21
C---- Enters data into the "iterative summary" file.
C     (This is version 2 of ERENI.)
C     !DASH
      save
C     !DASH
      real*8 CORE, EP, RHO, RHOWT, S, TAU, TE, WBD, WEP, WPOP, XJBR, Z
      integer I, IL, IOVER, ISUB, IU, JBD, JRHO, LUIS, METEP, METSE, N
C     !COM
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C     !DASH
      external HI, BYE
C
C               Z(N), TE(N), TAU(N), EP(N), RHO(N), XJBR(N), RHOWT(N),
      dimension Z(*), TE(*), TAU(*), EP(*), RHO(*), XJBR(*), RHOWT(*),
C
C               S(N)
     $          S(*)
C
      call HI ('ERENI')
C     !BEG
      write (LUIS,100) HEAD
  100 format(A80)
C
      write (LUIS,101) IOVER,ISUB,N,IU,IL,CORE
  101 format(' ',5X,'IOVER',6X,'ISUB',9X,'N',8X,'IU',8X,'IL',12X,'CORE'/
     $       ' ',5I10,1PE16.8)
      write (LUIS,102) WPOP,WBD,WEP,JRHO,JBD,METSE,METEP
  102 format(' ',6X,'WPOP',7X,'WBD',7X,'WEP',6X,'JRHO',7X,'JBD',5X,
     $           'METSE',5X,'METEP'/
     $       ' ',3F10.4,4I10)
C
      write (LUIS,103)
  103 format(5X,14X,'Z',13X,'TE',8X,'EPSILON',14X,'S',12X,'TAU',12X,
     $          'RHO',10X,'RHOWT',11X,'JBAR')
      write (LUIS,104) (I,Z(I),TE(I),EP(I),S(I),TAU(I),RHO(I),RHOWT(I),
     $                  XJBR(I),I=1,N)
  104 format(I5,1P8E15.7)
C     !END
      call BYE ('ERENI')
C
      return
      end
