      subroutine ORINOCO
     $(NO,N,EP,DEL,B,BS,BA,BF,S,TAU,RHO,XJBAR,FNDT,INCI,SHORT,
     $ IDDL,DELLM)
C
C     Rudolf Loeser, 1981 Aug 06
C---- Prints results of Line Source Function calculation.
C     (This is version 3 of ORINOCO.)
C     !DASH
      save
C     !DASH
      real*8 B, BA, BF, BS, DEL, DELLM, EP, FNDT, ONE, RHO, S, TAU,
     $       XJBAR, ZERO
      integer I, IDDL, N, NO
      logical DIDIT, INCI, SHORT
      character BLANK*1, EBA*11, EBF*11, EBS*11, EDL*11, EEP*10, ES*16,
     $          FNC*12
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external VECOUT, ORANJE, NUFU, SHIM, LINER, HI, BYE
C
C               EP(N), DEL(N), B(N), BS(N), BA(N), BF(N), S(N), RHO(N),
      dimension EP(*), DEL(*), B(*), BS(*), BA(*), BF(*), S(*), RHO(*),
C
C               TAU(N), XJBAR(N), FNDT(N)
     $          TAU(*), XJBAR(*), FNDT(*)
C
      call HI ('ORINOCO')
C     !BEG
C
      if(NO.gt.0) then
        if(SHORT) then
          call VECOUT (NO, TAU, N, 'Tau')
          call VECOUT (NO, S,   N, 'S'  )
          goto 110
        end if
C     !EJECT
        DIDIT = .false.
        call ORANJE (NO, INCI)
C
        do 108 I = 1,N
          if((I.lt.N).and.(I.eq.IDDL)) then
            call LINER (1, NO)
            write (NO,100)
  100       format(' ',5X,'DIRECT solution below this point.')
            call LINER (1, NO)
            DIDIT = .true.
          end if
          EEP = BLANK
          if(EP(I).ne.ZERO) then
                                write (EEP,101) EP(I)
          end if
          EDL = BLANK
          if(DEL(I).ne.ZERO) then
            if(DEL(I).le.ONE) then
                                write (EDL,102) DEL(I)
            else
                                write (EDL,103) DEL(I)
            end if
          end if
          EBS = BLANK
          if(BS(I).ne.ZERO) then
                                write (EBS,104) BS(I)
          end if
          EBA = BLANK
          EBF = BLANK
          if(.not.DIDIT) then
            if(BA(I).ne.ZERO) then
                                write (EBA,104) BA(I)
            end if
            if(BF(I).ne.ZERO) then
                                write (EBF,104) BF(I)
            end if
          end if
          write (ES,105) S(I)
          FNC = BLANK
          if(INCI) then
            if(FNDT(I).ne.ZERO) then
                                write (FNC,106) FNDT(I)
            end if
          end if
  101     format(1PE10.2)
  102     format(1PE11.3)
  103     format(' ****',F6.2)
  104     format(1PE11.2)
  105     format(1PE16.7)
  106     format(1PE12.4)
C     !EJECT
          if(DIDIT) then
            call NUFU  (I, B(I), BS(I), EBS, ZERO,  EBA, ZERO,  EBF,
     $                  S(I), ES)
          else
            call NUFU  (I, B(I), BS(I), EBS, BA(I), EBA, BF(I), EBF,
     $                  S(I), ES)
          end if
C
          write (NO,107) I,EEP,EDL,B(I),EBS,EBA,EBF,ES,TAU(I),RHO(I),
     $                   XJBAR(I),FNC
  107     format(' ',I3,A10,A11,1PE12.3,3A11,A16,E10.2,E10.2,E10.2,A12)
C
          call SHIM  (I,5,NO)
  108   continue
C
        if(DIDIT) then
          call LINER (1, NO)
          write (NO,109) DELLM
  109     format(' ',6X,'DELLIM =',1PE10.3,3X,'(To avoid the DIRECT ',
     $               'solution, set DELLIM = 0.)')
        end if
      end if
C
  110 continue
C     !END
      call BYE ('ORINOCO')
C
      return
      end
