      subroutine SULTANA
     $(HYDR,IONST,DUMP)
C
C     Rudolf Loeser, 2006 May 02
C---- Sets up CI & CE calculations controls.
C     !DASH
      save
C     !DASH
      integer I, IONST, IXNCS, LUEO
      logical DUMP, HYDR
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
      equivalence (KZQ(144),IXNCS)
C
C---- TANGELO     as of 2007 Mar 26
      integer     LCISW,NCISW,KCISW,KCISWD,LCESW,NCESW,KCESW,KCESWD
      parameter   (LCISW=6, LCESW=7)
      dimension   KCISW(LCISW), KCISWD(LCISW)
      dimension   KCESW(LCESW), KCESWD(LCESW)
      common      /TANGELO1/ NCISW,KCISW,KCISWD
      common      /TANGELO2/ NCESW,KCESW,KCESWD
C     Control switches for default CI & CE calculations.
C CI: 1 SHAH, 2 AR, 3 VORONOV, 4 VS, 5 JOHNSON, 6 CLARK
C CE: 1 SCHOLZ, 2 PB, 3 VS, 4 JOHNSON, 5 SEATON, 6 VREGE, 7 AGGRWL
C     (The default configurations are set up in subroutine SULTANA.)
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external  SNAIL, MESHED, MASHED, HI, BYE
      intrinsic max
C
      call HI ('SULTANA')
C     !BEG
C---- Set up DEFAULT methods
      if(HYDR) then
        KCISWD(1) = 1
        KCISWD(6) = 1
C
        KCESWD(7) = 1
        KCESWD(4) = 1
      else
        KCISWD(2) = 1
        KCISWD(6) = 1
C
        if(IONST.eq.1) then
          KCESWD(5) = 1
        end if
        KCESWD(6) = 1
      end if
C---- Set up methods USED by this run
      call SNAIL    (NCISW, KCISW, KCISWD)
      call SNAIL    (NCESW, KCESW, KCESWD)
C     !EJECT
      if(DUMP) then
        call MESHED ('SULTANA', 2)
        write (LUEO,100) HYDR,IONST,IXNCS
  100   format(' ','CI & CE method controls',10X,'HYDR =',L3,5X,
     $             'IONSTAGE =',I3,5X,'IXNCS =',I2//
     $         ' ',10X,' KCISWD  KCISW',10X,' KCESWD  KCESW')
        do 103 I = 1, max(NCISW,NCESW)
          if(I.gt.NCISW) then
            write (LUEO,101) KCESWD(I),KCESW(I)
  101       format(' ',34X,2I7)
          else if(I.gt.NCESW) then
            write (LUEO,102) KCISWD(I),KCISW(I)
  102       format(' ',10X,2I7,10X,2I7)
          else
            write (LUEO,102) KCISWD(I),KCISW(I),KCESWD(I),KCESW(I)
          end if
  103   continue
        if(HYDR) then
          if(IXNCS.eq.1) then
            write (LUEO,104) IXNCS,' '
  104       format(' ','IXNCS =',I2,' means: lowering of the ',
     $                 'ionization potential is',A,'used')
          else if(IXNCS.eq.0) then
            write (LUEO,104) IXNCS,' not '
          else
            write (LUEO,105) IXNCS
  105       format(' ','&&&&&  N O T E : IXNCS =',I12,' is crazy!')
          end if
        end if
        call MASHED ('SULTANA')
      end if
C     !END
      call BYE ('SULTANA')
C
      return
      end
