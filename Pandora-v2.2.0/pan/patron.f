      subroutine PATRON
     $(LU,LUAP,DMPA,LUVP,DMPV,LUNP,DMPN)
C
C     Rudolf Loeser, 1999 Jul 19
C---- Sets up logical output units and switches for TARPON.
C     !DASH
      save
C     !DASH
      integer IQA1D, IQA1P, IQADP, IQAMB, IQVDP, IQVLP, LU, LUAP, LUEO,
     $        LUNP, LUVP, MO
      logical DMPA, DMPN, DMPV
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
      equivalence (IQQ(267),IQA1P)
      equivalence (IQQ(264),IQAMB)
      equivalence (IQQ(220),IQADP)
      equivalence (IQQ(268),IQA1D)
      equivalence (IQQ(239),IQVLP)
      equivalence (IQQ(222),IQVDP)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external ZEUS, MESHED, MASHED, HI, BYE
C     !EJECT
C
      call HI ('PATRON')
C     !BEG
      LUAP = 0
      DMPA = .false.
      LUVP = 0
      DMPV = .false.
      LUNP = 0
      DMPN = .false.
C
      if(LU.gt.0) then
        call ZEUS   (LU, IQAMB, LUAP)
        if(IQADP.gt.0) then
          DMPA = .true.
        end if
C
        call ZEUS   (LU, IQVLP, LUVP)
        if(IQVDP.gt.0) then
          DMPV = .true.
        end if
C
        call ZEUS   (LU, IQA1P, LUNP)
        if(IQA1D.gt.0) then
          DMPN = .true.
        end if
      end if
C
      if(MO.gt.0) then
        call MESHED ('PATRON', 3)
        write (LUEO,100) LU,LUAP,DMPA,LUVP,DMPV,LUNP,DMPN
  100   format(' ','Status of output switches'/
     $         ' ',8X,'LU'  ,6X,'LUAP',6X,'DMPA',6X,'LUVP',6X,'DMPV',
     $             6X,'LUNP',6X,'DMPN'/
     $         ' ',I10,3(I10,L10))
        call MASHED ('PATRON')
      end if
C     !END
      call BYE ('PATRON')
C
      return
      end
