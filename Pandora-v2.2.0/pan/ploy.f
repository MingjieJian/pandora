      subroutine PLOY
     $(NO,EMU,EMUF,WMUF,FRR,ISSV,VXN,HND,HNDF,FNH)
C
C     Rudolf Loeser, 1980 Oct 24
C---- Prints data for Spectrum Calculations.
C     (This is version 4 of PLOY.)
C     !DASH
      save
C     !DASH
      real*8 CVXF, CVXM, EMU, EMUF, FNH, FRR, HND, HNDF, SCPS, SCVA,
     $       SCVB, SCVS, VXN, WMUF, YFLUX
      integer IQABS, IQCFX, IQECL, IQEMI, IQEMS, IQFLW, IQLGT, IQORI,
     $        IQSFO, IQSFS, IQTAS, IQWNM, ISSV, J, L, LF, LOGAS, MRR, N,
     $        NFB, NFH, NO, NOTA, NOTX, NVX
      logical CALVAX, CONTR, ECLIPSE, ORIG, PLANE, PROFILE, SPHERE, ZVX
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 7),L  )
      equivalence (JZQ(19),LF )
      equivalence (JZQ(15),MRR)
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(42),NVX)
      equivalence (JZQ(31),NFH)
      equivalence (JZQ( 9),NFB)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 30),YFLUX)
      equivalence (KZQ(138),LOGAS)
      equivalence (RZQ(170),SCVA )
      equivalence (RZQ(171),SCVS )
      equivalence (RZQ(174),SCVB )
      equivalence (RZQ(175),SCPS )
      equivalence (RZQ( 65),CVXM )
      equivalence (RZQ( 67),CVXF )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(27),NOTA )
      equivalence (LEST(25),NOTX )
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
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ(  8),IQSFO)
      equivalence (IQQ(  9),IQLGT)
      equivalence (IQQ( 55),IQEMI)
      equivalence (IQQ( 80),IQCFX)
      equivalence (IQQ(  6),IQECL)
      equivalence (IQQ(101),IQORI)
      equivalence (IQQ(335),IQFLW)
      equivalence (IQQ( 59),IQABS)
      equivalence (IQQ(104),IQEMS)
      equivalence (IQQ(105),IQTAS)
      equivalence (IQQ(290),IQWNM)
C     !DASH
      external PADMA, PRIVET, DOBRAWA, LINER, NAUGHTD, HI, BYE
C
C               ISSV(NVX), EMUF(LF), HNDF(NFH), VXN(N,NVX), WMUF(LF),
      dimension ISSV(*),   EMUF(*),  HNDF(*),   VXN(N,*),   WMUF(*),
C
C               EMU(L), FRR(MRR), HND(N), FNH(NFH)
     $          EMU(*), FRR(*),   HND(*), FNH(*)
C
      call HI ('PLOY')
C     !BEG
      if(NO.gt.0) then
C
        CALVAX  = ISSV(1).gt.0
        SPHERE  = (IQSFS.gt.0).or.(IQSFO.gt.0)
        PLANE   = .not.SPHERE
        PROFILE = (NOTA.gt.0)
        ECLIPSE = (NOTX.gt.0)
        CONTR   = (IQABS.gt.0).or.(IQEMS.gt.0).or.(IQTAS.gt.0)
        ORIG    = (IQORI.gt.0).and.((IQLGT.gt.0).or.(IQEMI.gt.0))
C
        call PADMA (NO, 'Spectrum Calculations')
C     !EJECT
        if(PLANE) then
          if((IQLGT.gt.0).and.PROFILE) then
            call LINER    (2, NO)
            write (NO,100)
  100       format(' ','MUF, values of Mu for Flux Profiles ',
     $                 'calculations')
            call PRIVET   (NO, EMUF, LF)
            if(LF.gt.1) then
              call LINER  (2, NO)
              write (NO,101)
  101         format(' ','WMUF, associated integration weights')
              call PRIVET (NO, WMUF, LF)
            end if
          end if
          if(((IQLGT.gt.0).and.PROFILE).or.(IQEMI.gt.0)) then
            call LINER    (2, NO)
            write (NO,102) LOGAS
  102       format(' ','MU, values of Mu for which Intensities are ',
     $                 'printed (LOGAS =',I2,')')
            call PRIVET   (NO, EMU, L)
          end if
        end if
C
        if((IQCFX.gt.0).and.PLANE) then
          call LINER      (2, NO)
          write (NO,103) YFLUX
  103     format(' ','YFLUX, Y for Continuum Flux ',1PE12.4)
        end if
C
        if(ECLIPSE.or.(IQECL.gt.0).or.SPHERE) then
          call LINER      (2, NO)
          write (NO,104)
  104     format(' ','FRR, values of radius fraction for Disk Flux ',
     $               'calculation')
          call PRIVET     (NO, FRR, MRR)
        end if
C     !EJECT
        if(NVX.gt.0) then
          call LINER      (2, NO)
          if(IQFLW.gt.0) then
            write (NO,105) NVX,CVXM,CVXF
  105       format(' ','Flow-broadening velocities:',I3,' sets based ',
     $                 'on CVXM =',1PE12.4,'  and CVXF =',E12.4)
          else
            if(CALVAX) then
              write (NO,106) 'shock',NVX
  106         format(' ','Additional ',A,' velocities: number of ',
     $                   'sets specified, NVX =',I3)
            else
              write (NO,106) 'expansion',NVX
            end if
          end if
C
          call NAUGHTD    (VXN(1,1), 1, N, ZVX)
          if(.not.ZVX) then
            do 108 J = 1,NVX
              call LINER    (1, NO)
              write (NO,107) J
  107         format(' ','Set # ',I2)
              call PRIVET (NO, VXN(1,J), N)
  108       continue
            call LINER    (1,NO)
            write (NO,109)
  109       format(' ','HND, total hydrogen number density')
            call PRIVET   (NO, HND, N)
          end if
C
          if(CALVAX) then
            call LINER    (1, NO)
            write (NO,110) SCVA,SCVS,SCVB,SCPS
  110       format(' ','Shock velocity amplitude SCVA =',1PE14.6,
     $                 ', scale height SCVS =',E14.6/
     $             ' ','                         SCVB =',E14.6,
     $                 ',              SCPS =',E14.6//
     $             ' ','The shock position indices are:')
            write (NO,111) (ISSV(J),J=1,NVX)
  111       format(' ',5I12,5X,5I12)
          else
            if((NFH.gt.0).and.(IQFLW.gt.0)) then
              call LINER  (1, NO)
              write (NO,112)
  112         format(' ','HNDF, hydrogen density table for FNH')
              call PRIVET (NO, HNDF, NFH)
              call LINER  (1, NO)
              write (NO,113)
  113         format(' ','FNH, standard table of chromosphere and ',
     $                   'transition region flow velocity')
              call PRIVET (NO, FNH, NFH)
            end if
          end if
C
        end if
C     !EJECT
        if(ORIG.or.CONTR) then
          call LINER   (2, NO)
          write (NO,114)
  114     format(' ','Concerning the ORIGINS and/or CONTRIBUTORS ',
     $               'printouts --')
          call LINER   (1, NO)
          call DOBRAWA (NO, (IQWNM.gt.0))
        end if
      end if
C     !END
      call BYE ('PLOY')
C
      return
      end
