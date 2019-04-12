      subroutine HIGRA
     $(KK,XK,GK,KOLEV,MR,RRNU,NSL,N,RK,RL,XNU,XNUC,WRAT)
C
C     Rudolf Loeser, 1998 Nov 03
C---- Set up default values of the Lyman XK table, and
C     sets the corresponding GK table = 0 (for HADRE).
C
C     A L S O :
C
C     Fudges RRNU to make it different from XK (to result in
C     distinct wavelengths for Continuum Data Blocks).
C
C     A L S O :
C
C     Adjusts the edge values of RRNU in any event.
C
C     A L S O :
C
C     Recomputes WRATs corresponding to changed RRNUs.
C     !DASH
      save
C     !DASH
      real*8 FACRN, FACXK, GK, HUNDRD, ONE, RK, RL, RRNU, TEN, WRAT, XK,
     $       XNU, XNUC, ZERO
      integer I, IQLYM, IS, J, KK, KOLEV, L, LUEO, MR, N, NSL, jummy
      logical ZRK, ZRL, ZXK
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
      equivalence (IQQ( 13),IQLYM)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C     !DASH
      external  NAUGHTD, MOVE1, MESHED, ABORT, RAISES, ZERO1, NOMAD,
     $          MINK, HI, BYE
      intrinsic min
C
C               MRZ = MRS+NSL+1
C
C               WRAT(MRZ), MR(NSL+1), RRNU(MRZ), RK(N,NSL), RL(N,NSL),
      dimension WRAT(*),   MR(*),     RRNU(*),   RK(N,*),   RL(N,*),
C
C               GK(KK), XNU(NSL), XK(KK), XNUC(NSL)
     $          GK(*),  XNU(*),   XK(*),  XNUC(*)
C
      data HUNDRD /1.0D2/
C
      call HI ('HIGRA')
C     !BEG
      FACRN = ONE+WAVEDEL*HUNDRD
      FACXK = ONE+WAVEDEL*TEN
C     !EJECT
      if(IQLYM.gt.0) then
        L = min(KK,(MR(KOLEV)+1))
        call MINK       (KOLEV, MR, jummy, IS)
C
        call NAUGHTD    (XK, 1, KK, ZXK)
        if(ZXK) then
          if(L.eq.1) then
            XK(1) = ONE
            GK(1) = ZERO
          else if(L.gt.0) then
            call MOVE1  (RRNU(IS), L, XK)
            call ZERO1  (GK, L)
          else
            call MESHED ('HIGRA', 1)
            write (LUEO,100) L,KK,KOLEV,MR(KOLEV)
  100       format(' ','Error in setting up default Lyman XK table.'//
     $             ' ','L =',I10,10X,'KK =',I10,10X,'KOLEV =',I10,10X,
     $                 'MR(KOLEV) =',I10)
            call ABORT
          end if
        end if
C
        if(L.gt.0) then
C----     Fudge RRNU if necessary
          call NAUGHTD  (RK(1,KOLEV), 1, N, ZRK)
          call NAUGHTD  (RL(1,KOLEV), 1, N, ZRL)
          if(ZRK.or.ZRL) then
C----       Make sure no RRNU value equals any XK value
C           (The RRNU values of level KOLEV are used only once, in
C           the first overall iteration, to get initial RK and/or RL.)
            call RAISES (WAVEDEL, KOLEV, RRNU(IS), WRAT, XNU, XNUC,
     $                   L, FACRN, XK, KK)
          end if
C----     Fudge edge values in any event
          XK(1)    = FACXK
          RRNU(IS) = FACRN
          call NOMAD    (RRNU(IS), XNU, XNUC, KOLEV, WRAT(IS))
        end if
      end if
C
C---- Jiggle the RRNU edge values
      do 102 J = 1,NSL
        call MINK       (J, MR, jummy, IS)
        L = MR(J)+1
        do 101 I = 1,L
          if(RRNU(IS).eq.ONE) then
            RRNU(IS) = FACXK
            call NOMAD  (RRNU(IS), XNU, XNUC, J, WRAT(IS))
            goto 102
          end if
          IS = IS+1
  101   continue
  102 continue
C     !END
      call BYE ('HIGRA')
C
      return
      end
