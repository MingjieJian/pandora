      subroutine IMBROS
     $(NO,QELSM,IHSSW,IHSKM,IHSSM,IHSDP,IHSDD,MS,NS,N,HSBM,HSBFEQ,
     $ HSBDMX,HSBDMN,IHSSP,CSDW,FSTKM,FRCDL,FMCDL,IQSTW,LDLMX)
C
C     Rudolf Loeser, 1991 Dec 17
C---- Prints Hydrogen Stark broadening data.
C     !DASH
      save
C     !DASH
      real*8 CSDW, FMCDL, FRCDL, FSTKM, HSBDMN, HSBDMX, HSBFEQ, HSBM
      integer IHSDD, IHSDP, IHSKM, IHSSM, IHSSP, IHSSW, IQSTW, LDLMX,
     $        MS, N, NO, NS
      logical CONV, SPLT
      character QELSM*8
C     !DASH
      external  PADMA, LINER, HI, BYE
C
      call HI ('IMBROS')
C     !BEG
      if((NO.gt.0).and.(QELSM.eq.'H  ')) then
        CONV = IHSSW.gt.0
        SPLT = (IHSSP.gt.0).and.(LDLMX.gt.1)
C
        if(CONV.or.SPLT) then
          call PADMA     (NO,'Hydrogen Stark Broadening')
C
          call LINER     (1,NO)
          if(CONV) then
            write (NO,100) HSBM,HSBFEQ,IHSKM,HSBDMN,HSBDMX
  100       format(' ','Control parameters for convolution ',
     $                 'calculation:'/
     $             ' ','HSBM = script-M =',1PE10.3/
     $             ' ','HSBFEQ = accuracy limit =',E10.3,', interval ',
     $                 'refinement limited to IHSKM =',I5,' halvings'/
     $             ' ','HSBDMN = minimum subinterval =',E10.3,
     $                 ', HSBDMX = maximum subinterval =',E10.3)
            if((IHSDP.ge.1).and.(IHSDP.le.N)) then
              call LINER (1,NO)
              write (NO,101) IHSDP,IHSDD,MS,NS,IHSSM
  101         format(' ','Detailed printout may appear for the',I5,
     $                   '. depth and the',I4,'. frequency for ',
     $                   'transition',I3,'/',I2,'.'/
     $               ' ','Up to IHSSM =',I8,' integrand data sets ',
     $                   'can be saved and exhibited.')
            else
              write (NO,102)
  102         format(' ','No detailed printout has been requested.')
            end if
C
          else
            write (NO,103)
  103       format(' ','No explicitly-convolved profiles are ',
     $                 'computed (parameters IHSSW and CSTARK[u,l])')
          end if
C     !EJECT
          call LINER     (1,NO)
          if(SPLT) then
            write (NO,104) FSTKM,FRCDL,FMCDL,LDLMX
  104       format(' ','Control parameters for Stark line splitting ',
     $                 '(for detailed explanations, ',
     $                 'turn on option HSTSUMM).'/
     $             ' ','FSTKM =',1PE12.4,5X,'FRCDL =',E12.4,5X,
     $                 'FMCDL =',E12.4,5X,'LDLMX =',I5,10X,
     $                 'Note also option STKWATT.')
            if(IQSTW.gt.0) then
              write (NO,105) CSDW
  105         format(' ','CSDW  =',1PE12.4,' : number of Doppler ',
     $                   'widths from line center at which the '
     $                   'component strengths are reduced by the '
     $                   'factor 1/e.')
            end if
          else
            write (NO,106)
  106       format(' ','No Stark splitting is used (parameters ',
     $                 'IHSSP and LDLMAX)')
          end if
        end if
      end if
C     !END
      call BYE ('IMBROS')
C
      return
      end
