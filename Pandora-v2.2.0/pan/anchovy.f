      subroutine ANCHOVY
     $(LU,N,MN1,ALFINAL,HE1P,FACTOR,HE1,XN1,HEK,XNK,RABD)
C
C     Rudolf Loeser, 2002 Dec 16
C---- Prints, for NELKE.
C     (This is version 2 of ANCHOVY.)
C     !DASH
      save
C     !DASH
      real*8 ALFINAL, FACTOR, HE1, HE1P, HEK, RABD, XN1, XNK
      integer I, LU, MN1, N
      character ALF*15, BLANK*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external LINER,SHIM, HI, BYE
C
C               ALFINAL(MN1), HE1P(N), RABD(N), HE1(N), XN1(N), HEK(N),
      dimension ALFINAL(*),   HE1P(*), RABD(*), HE1(*), XN1(*), HEK(*),
C
C               XNK(N), FACTOR(N)
     $          XNK(*), FACTOR(*)
C
      call HI ('ANCHOVY')
C     !BEG
      if(LU.gt.0) then
        call LINER  (2,LU)
        write (LU,100)
  100   format(' ','R    = HEND / ( HE1(NEW) + N1 + NK )'
     $         ' ','RABD = ( N1 + NK ) / HEND'//
     $         ' ',8X,'alpha(final)',7X,'HE1(new)',14X,'R',12X,'HE1',
     $             13X,'N1',12X,'HEK',13X,'NK',11X,'RABD')
        call LINER  (1,LU)
        do 103 I = 1,N
          if(I.le.MN1) then
            write (ALF,101) ALFINAL(I)
  101       format(1PE15.7)
          else
            ALF = BLANK
          end if
          write (LU,102) I,ALF,HE1P(I),FACTOR(I),HE1(I),XN1(I),HEK(I),
     $                     XNK(I),RABD(I)
  102     format(' ',I5,A15,1P7E15.7)
          call SHIM (I,5,LU)
  103   continue
      end if
C     !END
      call BYE ('ANCHOVY')
C
      return
      end
