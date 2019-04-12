      subroutine PHOEBUS
C
C     Rudolf Loeser, 2006 Apr 11
C---- Reads CEMETHOD.
C     This is version 4 of PHOEBUS.)
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer KERR, KIND, L, LOOK, LUEO, MCEOF, MODE, jummy
      character BETH*8, METH*8, QNAME*9
C     !COM
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(223),MCEOF)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external CARMEN, KIWI, UNMIX, LOOKUC, MESHED, CHLOE, ABORT,
     $         MACE,  HI, BYE
C
      dimension METH(9)
C
      data METH /'SEATON  ', 'VREGE   ', 'SCHOLZ  ', 'PB      ',
     $           'VS      ', 'JOHNSON ', 'AGGRWL  ', 'ONTHEFLY',
     $           ')       '/
C
      call HI ('PHOEBUS')
C     !BEG
      KERR = 0
      call MACE
  100 continue
        call KIWI   (MODE, dummy, jummy, QNAME, jummy)
        if(MODE.ne.2) then
          goto 201
        end if
C
        L = +1
        BETH = QNAME(1:8)
        if(QNAME(1:1).eq.'-') then
          L = -1
          BETH = QNAME(2:9)
        end if
C
        call UNMIX  (BETH)
        call LOOKUC (METH, 9, BETH, KIND, LOOK)
        if(LOOK.ne.1) then
          goto 202
        end if
C
        goto (101, 102, 103, 104, 105, 106, 107, 108, 199), KIND
  101   continue
          KCESW(5) = L
          goto 100
  102   continue
          KCESW(6) = L
          goto 100
  103   continue
          KCESW(1) = L
          goto 100
  104   continue
          KCESW(2) = L
          goto 100
  105   continue
          KCESW(3) = L
          goto 100
  106   continue
          KCESW(4) = L
          goto 100
  107   continue
          KCESW(7) = L
          goto 100
  108   continue
          MCEOF = 1
          goto 100
C     !EJECT
C---- Error processing
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED   ('PHOEBUS', 1)
      write (LUEO, 200) METH
  200 format(' ','Trouble reading CEMETH data items. List of valid ',
     $           'items:'//
     $      (' ',4X,10A10))
      call CHLOE    (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !END
      call BYE ('PHOEBUS')
C
      return
      end
