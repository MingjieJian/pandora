      subroutine DELIA
     $(LU,JRHO,JBD,JBDNC,CWR,CHOP,CHLIM,ILI,NIL,XINCH,WMN,WMX,SMP,
     $ MBREC,CWJ)
C
C     Rudolf Loeser, 1987 Aug 18
C---- Prints "RHO and RBD" parameters.
C     !DASH
      save
C     !DASH
      real*8 CHLIM, CHOP, CWJ, CWR, SMP, WMN, WMX, XINCH
      integer ILI, JBD, JBDNC, JRHO, K, LU, MBREC, NIL
      character BT*3, RT*4
C     !DASH
      external  PADMA, LINER, HI, BYE
C
      dimension RT(4), BT(4)
C
      data RT /'RHOS', 'RHOJ', 'RHOW', ' ?? '/
      data BT /'BDJ' , 'BDR' , 'BDQ' , ' ? ' /
C
      call HI ('DELIA')
C     !BEG
      if(LU.gt.0) then
        call PADMA (LU, 'RHO and Ratio of Departure Coefficients')
        call LINER (1, LU)
C
        K = JRHO+1
        if((K.lt.1).or.(K.gt.3)) then
          K = 4
        end if
        write (LU,100) RT(K),JRHO
  100   format(' ','RHO-option is ',A4,2X,'(',I5,')')
C
        K = JBD+1
        if((K.lt.1).or.(K.gt.3)) then
          K = 4
        end if
        write (LU,101) BT(K),JBD
  101   format(' ','BD-option  is ',A3,3X,'(',I5,')')
        if(K.eq.3) then
          write (LU,102)
  102     format(' ',14X,'BDQ should not be used if some computed ',
     $               'values of line source function are negative.')
        end if
C
        call LINER (1, LU)
        if(MBREC.eq.1) then
          write (LU,103) MBREC, 'are'
  103     format(' ','MBREC =',I2,': B-values ',A,' edited to remain ',
     $               'consistent with recomputed ND-values.')
        else
          write (LU,103) MBREC, 'are not'
        end if
C     !EJECT
        call LINER (1, LU)
        write (LU,104)
  104   format(' ','Reminders: the "RHO AND RBD" printout (and ',
     $             'explanation) are controlled by RHBPRNT.'/
     $         ' ','The calculations themselves are also controlled ',
     $             'by options RHOWOPT, WATESTR, and RSMOOTH.')
C
        call LINER (1, LU)
        if(JBDNC.eq.1) then
          write (LU,105) JBDNC,'suppressed.'
  105     format(' ','JBDNC =',I3,' : analysis of alternate RHO+RBD ',
     $               'methods will be ',A)
        else
          write (LU,105) JBDNC,'done,'
        end if
C
        call LINER (1, LU)
        write (LU,106) CWR,CHOP,CHLIM,ILI,NIL,WMN,XINCH,WMX,CWJ,SMP
  106   format(' ','CWR  =',1PE11.4,5X,'CHOP =',E11.4,5X,
     $             'CHLIM =',E11.3,5X,'ILI =',I4,12X,'NIL =',I4/
     $         ' ','WRMN =',E11.4,5X,'INCH =',E11.4,5X,'WRMX  =',
     $             E11.4,5X,'CWJ =',E11.4,5X,'SMP =',E11.4)
C
      end if
C     !END
      call BYE ('DELIA')
C
      return
      end
