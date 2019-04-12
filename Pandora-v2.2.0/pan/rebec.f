      subroutine REBEC
     $(NO,IB,IE,DORK,DORL,IRLS,KSHL,KOOL,DETL,RKI,RKC,RLI,RLC,RKM,RKMC,
     $ RLA,RLAC,RLB,RLBC,RLM,RLMC,CKI,PKS,TREFF,PTRF,CP)
C
C     Rudolf Loeser, 1980 Mar 11
C---- Prints tables, for HULA.
C     !DASH
      save
C     !DASH
      real*8 CKI, CP, PKS, RKC, RKI, RKM, RKMC, RLA, RLAC, RLB, RLBC,
     $       RLC, RLI, RLM, RLMC, TREFF, ZERO
      integer I, IB, IE, IRLS, NO
      logical DETL, DORK, DORL, KBRF, KOOL, KSHL, PRAT, PRRK, PRRL,
     $        PTRF
      character BLANK*1, KRLA*1, KRLB*1, STAR*1, TCK*4, TRK*4, TRKC*4,
     $          TRKM*4, TRKMC*4, TRL*4, TRLA*4, TRLAC*4, TRLB*4,
     $          TRLBC*4, TRLC*4, TRLM*4, TRLMC*4, TTRK*4
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external MARKI, HI, BYE
C
C               RKI(N), RKC(N), RLI(N), RLC(N), RLA(N), RLB(N), PKS(N),
      dimension RKI(*), RKC(*), RLI(*), RLC(*), RLA(*), RLB(*), PKS(*),
C
C               RKM(N), RKMC(N), RLBC(N), RLMC(N), RLAC(N), TREFF(N),
     $          RKM(*), RKMC(*), RLBC(*), RLMC(*), RLAC(*), TREFF(*),
C
C               RLM(N), CKI(N)
     $          RLM(*), CKI(*)
C
      data TRK,  TRKC,  TRL,  TRLC,  TRKM,  TRKMC,  TRLA /
     $     'RK', 'RKC', 'RL', 'RLC', 'RKM', 'RKMC', 'RLA'/
C
      data TRLAC,  TRLB,  TRLBC,  TRLM,  TRLMC,  TCK,  TTRK /
     $     'RLAC', 'RLB', 'RLBC', 'RLM', 'RLMC', 'CK', 'TRK'/
C     !EJECT
C
      call HI ('REBEC')
C     !BEG
      KBRF = .not.DETL
      PRAT = CP.ne.ZERO
      PRRK = DORK.and.PRAT
      PRRL = DORL.and.PRAT
      if(IB.eq.1) then
        call MARKI (1, IRLS, KRLA, STAR, BLANK)
        call MARKI (2, IRLS, KRLB, STAR, BLANK)
      end if
C
  100 format(' ',31X,A1,A4,1P8E11.3)
C
      if(KSHL) then
                     write (NO,100) BLANK,TRK,  (PKS(I),  I=IB,IE)
        if(DETL)     write (NO,100) BLANK,TRKM, (RKM(I),  I=IB,IE)
      else
        if(PRRK) then
          if(PTRF)   write (NO,100) BLANK,TTRK, (TREFF(I),I=IB,IE)
                     write (NO,100) BLANK,TRK,  (RKI(I),  I=IB,IE)
          if(DETL)   write (NO,100) BLANK,TRKM, (RKM(I),  I=IB,IE)
          if(KOOL) then
                     write (NO,100) BLANK,TRKC, (RKC(I),  I=IB,IE)
            if(DETL) write (NO,100) BLANK,TRKMC,(RKMC(I), I=IB,IE)
          end if
        end if
        if(PRRL) then
          if(DETL)   write (NO,100) KRLA, TRLA, (RLA(I),  I=IB,IE)
          if(DETL)   write (NO,100) KRLB, TRLB, (RLB(I),  I=IB,IE)
          if(KBRF)   write (NO,100) BLANK,TRL,  (RLI(I),  I=IB,IE)
          if(DETL)   write (NO,100) BLANK,TRLM, (RLM(I),  I=IB,IE)
          if(KOOL) then
            if(DETL) write (NO,100) KRLA, TRLAC,(RLAC(I), I=IB,IE)
            if(DETL) write (NO,100) KRLB, TRLBC,(RLBC(I), I=IB,IE)
            if(KBRF) write (NO,100) BLANK,TRLC, (RLC(I),  I=IB,IE)
            if(DETL) write (NO,100) BLANK,TRLMC,(RLMC(I), I=IB,IE)
          end if
        end if
                     write (NO,100) BLANK,TCK,  (CKI(I),  I=IB,IE)
      end if
C     !END
      call BYE ('REBEC')
C
      return
      end
