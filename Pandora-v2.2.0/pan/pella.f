      subroutine PELLA
     $(NO,LC,LIMP,N,BD,IOVER,LINE,PRINTB)
C
C     Rudolf Loeser, 1974 May 23
C---- Prints departure coefficients.
C     See also MILDEW.
C     (This is version 2 of PELLA.)
C     !DASH
      save
C     !DASH
      real*8 BD, DEL, ONE
      integer I, IOVER, J, JE, JS, KEQ, KN, LC, LIMP, N, NO
      logical PRINTB
      character LINE*9, MODE*9
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  LINER, RANGED, KYNWYL, SHIM, HI, BYE
      intrinsic min
C
C               BD(N,LIMP)
      dimension BD(N,*)
C
      dimension LINE(*)
C
      data DEL /1.D-5/
C
      call HI ('PELLA')
C     !BEG
      call LINER (3,NO)
      write (NO,100) LC
  100 format(' ',41X,'D E P A R T U R E   C O E F F I C I E N T S',I43)
C     !EJECT
      if(PRINTB) then
        KN = N*LIMP
        call RANGED     (BD,1,KN,DEL,ONE,KEQ)
        if(KEQ.ne.KN) then
C
          call KYNWYL   (LINE,LIMP,1,MODE,IOVER,LC)
          JE = 0
  101     continue
            JS = JE+1
            JE = min(JE+8,LIMP)
C
            call LINER  (2,NO)
            write (NO,102) (J,J=JS,JE)
  102       format(' ',39X,8(:,'   Level',I3))
            write (NO,103) (LINE(J),J=JS,JE)
  103       format(' ',39X,8(2X,A9))
            call LINER  (1,NO)
C
            do 105 I = 1,N
              write (NO,104) I,(BD(I,J),J=JS,JE)
  104         format(' ',I3,36X,1P8E11.3)
              call SHIM (I,5,NO)
  105       continue
C
          if(JE.lt.LIMP) goto 101
C
        else
          call LINER    (1,NO)
          write (NO,106)
  106     format(' ',41X,'All equal to 1')
        end if
C
      else
        call LINER      (1,NO)
        write (NO,107)
  107   format(' ',41X,'All equal, to 8 figures, to the values for ',
     $             'the ion-of-the-run,'/
     $         ' ',41X,'which were already printed.')
      end if
C     !END
      call BYE ('PELLA')
C
      return
      end
