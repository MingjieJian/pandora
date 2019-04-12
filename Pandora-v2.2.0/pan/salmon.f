      subroutine SALMON
     $(IU,IL,WVLUL,CDWUL,K,XI,N,TE,BDI,F,FSAV,IMG,FO,EDITED,SLF,
     $ NO,KHED,KLAB)
C
C     Rudolf Loeser, 1992 Mar 20
C---- Computes the frequency-dependent Line Source Function, SLF,
C     for transition (IU,IL).
C     (This is version 2 of SALMON.)
C     !DASH
      save
C     !DASH
      real*8 BDI, CDWUL, CON65, F, FO, FSAV, ONE, SET, SLF, TE, WNK,
     $       WVLUL, XI, XLM, XNUM
      integer I, IL, IMG, IQESL, IU, J, K, KERM, KHED, KLAB, KMSS, MODE,
     $        N, NERM, NO
      logical BAD, EDITED
      character LABEL*90
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(145),IQESL)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external WANDA, KUYUK, EDITH, MOVE1, DIVIDE, RIGEL, TONG, HI, BYE
C
C               BDI(N,NL), SLF(N,K), IMG(N), TE(N), F(N), XI(K), FO(N),
      dimension BDI(N,*),  SLF(N,*), IMG(*), TE(*), F(*), XI(*), FO(*),
C
C               FSAV(N,K)
     $          FSAV(N,*)
C
      data KERM,NERM,MODE /0, 1000, 2/
C
      call HI ('SALMON')
C     !BEG
      EDITED = .false.
      KMSS   = 0
      do 103 J = 1,K
        XLM = WVLUL+CDWUL*XI(J)
C
        call WANDA    (XLM, WNK)
        call RIGEL    (65, CON65)
        XNUM = CON65*(WNK**3)
C
C----   Get F table, and save it
        do 100 I = 1,N
          call KUYUK  (XLM, MODE, BDI(I,IU), BDI(I,IL), TE(I), SET)
          call DIVIDE (ONE, SET, F(I))
  100   continue
        call MOVE1    (F, N, FSAV(1,J))
C
        if(IQESL.gt.0) then
C----     Edit F table
          write (LABEL,101) IU,IL,J,XI(J)
  101     format('[BD(l)/BD(u)] * exponential term, for SLF (',I3,
     $           '/',I3,') at frequency XI(',I3,') =',1PE10.3,5X,
     $           '(Option SEDITIF)')
          call EDITH    (F, N, ONE, 2, 1, KMSS, LABEL, IMG, FO, KERM,
     $                   NERM, BAD)
          if(BAD) then
            EDITED = .true.
            if(KMSS.eq.0) then
              KMSS = 1
C----         Provide the error printout that was skipped so that a
C             section header could be assured
              call TONG (NO, KHED, F, N, ONE, 2, 1, KMSS, LABEL, IMG,
     $                   FO, KERM, NERM, KLAB)
            end if
          end if
        end if
C
C----   Now compute SLF
        do 102 I = 1,N
          call DIVIDE (XNUM, (F(I)-ONE), SLF(I,J))
  102   continue
  103 continue
C     !END
      call BYE ('SALMON')
C
      return
      end
