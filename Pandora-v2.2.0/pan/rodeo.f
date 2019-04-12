      subroutine RODEO
     $(LU,BANDL,BANDU,BANDY,IBNDE,NAB,KWC,NCP,NKA,ZALBK,ALBK,KWA,NCQ,
     $ CQT,CQA)
C
C     Rudolf Loeser, 1983 Jun 29
C---- Prints "Line Opacity" data.
C     !DASH
      save
C     !DASH
      real*8 ALBK, ALOMA, ALOMI, BANDL, BANDU, BANDY, CLM, CQA, CQM,
     $       CQT, CURMA, CURMI, PNH, ZALBK, ZERO
      integer I, IBNDE, IQCOA, J, KAVNP, KAVNT, KAVNZ, KCOAA, KOMNP,
     $        KOMNT, KOMNV, KURIN, KWA, KWC, LU, NAB, NCP, NCQ, NKA
      logical AVERAG, COMPOS, LINEOP, STATST
      character BLANK*1, ECLIP*10, LABEL*10
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 69),KOMNP)
      equivalence (KZQ( 70),KOMNT)
      equivalence (KZQ( 68),KOMNV)
      equivalence (KZQ(141),KAVNT)
      equivalence (KZQ(142),KAVNP)
      equivalence (KZQ(143),KAVNZ)
      equivalence (KZQ( 27),KURIN)
      equivalence (RZQ( 48),CURMI)
      equivalence (RZQ( 49),CURMA)
      equivalence (RZQ(142),ALOMI)
      equivalence (RZQ(143),ALOMA)
      equivalence (RZQ(101),CQM  )
      equivalence (RZQ(150),PNH  )
      equivalence (RZQ(151),CLM  )
      equivalence (KZQ(185),KCOAA)
C
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
C     !EJECT
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
      equivalence (IQQ(320),IQCOA)
C     !DASH
      external  SABLE, PADMA, LINER, HAKO, HI, BYE
      intrinsic min, max
C
C               BANDL(NAB), BANDU(NAB), BANDY(NAB), CQT(NCQ), CQA(NCQ),
      dimension BANDL(*),   BANDU(*),   BANDY(*),   CQT(*),   CQA(*),
C
C               ZALBK(NKA), ALBK(NKA), IBNDE(NAB)
     $          ZALBK(*),   ALBK(*),   IBNDE(*)
C
      dimension ECLIP(2)
C
      data ECLIP /'          ', '   Eclipse'/
C
      call HI ('RODEO')
C     !BEG
      if(LU.gt.0) then
        call SABLE    (STATST, COMPOS, AVERAG, LINEOP)
        if(LINEOP) then
          call PADMA  (LU, 'Line Opacities')
        end if
C     !EJECT
        if(COMPOS) then
          call LINER  (1, LU)
          write (LU,100) KWC,KOMNP,KOMNT,KOMNV
  100     format(' ',3X,'Composite Line Opacity:'//
     $           ' ','The compressed data file has',I5,' wavelengths,'/
     $           ' ','with ',I4,' Pressure grid points, ',I4,
     $               ' Temperature grid points and ',I4,' Velocity '
     $               ' grid points per wavelength.')
          call LINER  (1, LU)
          if(NAB.eq.1) then
            write (LU,101) 'band'
  101       format(' ','The following wavelength ',A,' will be used:')
          else
            write (LU,101) 'bands'
          end if
          call LINER  (1, LU)
          do 103 I = 1,NAB
            J = max(min((IBNDE(I)+1),2),1)
            call HAKO (BANDY(I), LABEL)
            write (LU,102) I,BANDL(I),BANDU(I),LABEL,ECLIP(J)
  102       format(' ',I10,2F12.5,2X,2A10)
  103     continue
          call LINER  (1, LU)
          if(NAB.eq.1) then
            write (LU,104) 'This band contains', NCP
  104       format(' ',A,I8,' wavelength data points.')
          else
            write (LU,104) 'These bands contain a total of', NCP
          end if
          call LINER  (1, LU)
          if(IQCOA.le.0) then
            write (LU,105)
  105       format(' ','To get an analysis of the relative ',
     $                 'contribution of the Composite Lines to the ',
     $                 'total background opacity,'/
     $             ' ','turn on option COMOPAN.')
          else
            write (LU,106)
  106       format(' ','Since option COMOPAN is on, an analysis of ',
     $                 'the Composite Lines opacity contribution will ',
     $                 'be produced.')
            if(KCOAA.le.0) then
              write (LU,107)
  107         format(' ','For an abbreviated printout format of this ',
     $                   'analysis, set KCOAA = 1.')
            else
              write (LU,108)
  108         format(' ','To get the detailed printout of this ',
     $                   'analysis, set KCOAA = 0 (default).')
            end if
          end if
        end if
C     !EJECT
        if(STATST) then
          call LINER  (2, LU)
          write (LU,109) KURIN,CURMI,CURMA
  109     format(' ',3X,'Statistical Line Opacity:'//
     $           ' ','Selection index KURIN =',I3,10X,
     $               'Wavelength range from ',1PE16.8,
     $               ' to ',E16.8,' Angstroms.')
        end if
        if(AVERAG) then
          call LINER  (2, LU)
          write (LU,110) KWA,ALOMI,ALOMA
  110     format(' ',3X,'Averaged Line Opacity:'//
     $           ' ','The wavelength table has',I6,' entries, ',
     $               'from',1PE16.9,' to',E16.9,' Angstroms.')
          if(KAVNZ.gt.1) then
            write (LU,111) KAVNZ,' Depth'
  111       format(' ',I3,A,' grid points have been specified.')
          else
            if(KAVNT.gt.1) then
              write (LU,111) KAVNT,' Temperature'
            end if
            if(KAVNP.gt.1) then
              write (LU,111) KAVNP,' Pressure'
            end if
          end if
        end if
        if(LINEOP) then
          call LINER   (2,LU)
          if(NKA.gt.0) then
            write (LU,112)
  112       format(' ',3X,'Scattering Albedo:'//
     $             ' ','Values of scattering albedo ',
     $                 'as a function of Z are obtained by ',
     $                 'linear interpolation from:'//
     $             ' ',14X,'ZALBK',6X,'ALBK'/)
            write (LU,113) (I,ZALBK(I),ALBK(I),I=1,NKA)
  113       format(' ',I5,1PE14.6,0PF10.5)
            call LINER (1, LU)
            write (LU,114)
  114       format(' ','For values of Z falling beyond the ends of ',
     $                 'the ZALBK set, the respective end values of ',
     $                 'the ALBK set are used.')
            call LINER (2, LU)
            write (LU,115)
  115       format(' ','To use various formulas for the albedo as ',
     $                 'selected by the value of CQM, set NKA = 0.')
          else
C     !EJECT
            if(CQM.gt.ZERO) then
              write (LU,116) CLM,PNH,BLANK
  116         format(' ',3X,'Scattering Albedo = 1 / [1 + Q(i)]'//
     $               ' ',T10,'where'//
     $               ' ',T10,'Q(i) = Xi(i) * [ 1 + CLM * (10000/LM) ] ',
     $                   '+ PNH * [ NH(i)/NH(N) ],'//
     $               ' ',T15,'LM is in Angstroms, CLM =',1PE10.2,
     $                   ', PNH =',E10.2,', and'//
     $               ' ',T10,'Xi(i) = [ NE(i) / 1.07E14 ] * ',
     $                  '(LM / 5000)**3 / CQM',A)
              call LINER (1, LU)
              write (LU,117) CQM
  117         format(' ',T10,'and CQM =',1PE12.4//
     $               ' ',T15,'Formula for Xi(i) from: Anderson, L.S. '
     $                   '(1989), Astrophys.J., eqn 36.')
              call LINER (2, LU)
              write (LU,118)
  118         format(' ','To use a table of CQM(temp), set the input ',
     $                   'constant CQM = 0.')
            else
              write (LU,116) CLM,PNH,'(TE(i))'
              call LINER (1, LU)
              write (LU,119)
  119         format(' ',T10,'and CQM(temp) is obtained by ',
     $                   'interpolation from the following table:')
              call LINER (1, LU)
              write (LU,120)
  120         format(' ',22X,'temp',13X,'CQM')
              call LINER (1, LU)
              write (LU,121) (I,CQT(I),CQA(I),I=1,NCQ)
  121         format(5(' ',I10,1P2E16.4/))
              call LINER (1, LU)
              write (LU,122)
  122         format(' ',T15,'Formula for Xi(i) from: Anderson, L.S. ',
     $                   '(1989), Astrophys.J., eqn 36.')
              call LINER (2, LU)
              write (LU,123)
  123         format(' ','To use a temperature-independent value of ',
     $                   'CQM, supply a non-zero input value.')
            end if
            call LINER   (2, LU)
            write (LU,124)
  124       format(' ','To use tabulated albedo values, supply values ',
     $                 'of NKA, ZALBK, and ALBK.')
          end if
        end if
      end if
C     !END
      call BYE ('RODEO')
C
      return
      end
