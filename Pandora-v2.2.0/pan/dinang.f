      subroutine DINANG
     $(OBL,ILFLX)
C
C     Rudolf Loeser, 1981 Dec 08
C---- Produces a Orion Data Blocks dump.
C     !DASH
      save
C     !DASH
      real*8 OBL
      integer I, ICE, ILFLX, IPR01, IPR02, LOALL, LOBC, LOCWT, LOJNU,
     $        LOKPC, LONAM, LOPHI, LOPHIW, LOSIG, LOT1, LOT2, LOT3,
     $        LOTNU, LOWH, LOWN, LOWWT, LOXLL
      logical KILROY
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
      equivalence (KZQ( 57),IPR01)
      equivalence (KZQ( 58),IPR02)
C
C---- URANUS      as of 2005 Dec 12
      integer     LEMUR
      parameter   (LEMUR=30000)
      integer     LIMPID,IUOP,ILOP,NBOP,MBOP,KEROP,INDOP
      real*8      OPNAM
      dimension   OPNAM(LEMUR),KEROP(LEMUR),INDOP(LEMUR)
      common      /URANUS1/ LIMPID,IUOP,ILOP,NBOP,MBOP
      common      /URANUS2/ KEROP
      common      /URANUS3/ INDOP
      common      /URANUS4/ OPNAM
C     Record index for Diana/Orion Data Blocks.
C
C     KEROP .eq. 0: acceptable data block, use it;
C     KEROP .gt. 0: defective data block, do not use it.
C     (The various values of KEROP signify different error conditions).
C     .
C---- ORIBLOC     as of 2005 Jan 21
      integer     LODLEN,LOD
      dimension   LOD(17)
      common      /ORIBLOC/ LODLEN,LOD
C     This is the "ORION" Data Block index, for the calculation
C     of line source functions in an expanding atmosphere.
C     .
      equivalence
     $(LOD( 1),LONAM ),(LOD( 2),LOPHIW),(LOD( 3),LOKPC ),
     $(LOD( 4),LOPHI ),(LOD( 5),LOCWT ),(LOD( 6),LOWN  ),
     $(LOD( 7),LOBC  ),(LOD( 8),LOTNU ),(LOD( 9),LOJNU ),
     $(LOD(10),LOT1  ),(LOD(11),LOSIG ),(LOD(12),LOALL ),
     $(LOD(13),LOXLL ),(LOD(14),LOWH  ),(LOD(15),LOWWT ),
     $(LOD(16),LOT2  ),(LOD(17),LOT3  )
C     .
C     !EJECT
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 4),ICE  )
C     !DASH
C     !EJECT
      external LEYTE, ARIKI, MUSHED, MASHED, HI, BYE
C
C               OBL(Lodlen)
      dimension OBL(*)
C
      call HI ('DINANG')
C     !BEG
      if(NBOP.gt.0) then
        KILROY = .true.
C
        do 100 I = 1,NBOP
          if((I.ge.IPR01).and.(I.le.IPR02)) then
C
            call LEYTE  (OBL, LODLEN, INDOP(I))
            call MUSHED ('DINANG', 2, KILROY)
            call ARIKI  (I, KEROP(I), ICE, ILFLX,
     $                   OBL(LONAM),  OBL(LOBC),   OBL(LOTNU),
     $                   OBL(LOJNU),  OBL(LOSIG),  OBL(LOT1),
     $                   OBL(LOT2),   OBL(LOT3),   OBL(LOWN),
     $                   OBL(LOPHIW), OBL(LOCWT),  OBL(LOPHI),
     $                   OBL(LOKPC),  OBL(LOALL),  OBL(LOXLL),
     $                   OBL(LOWH),   OBL(LOWWT))
          end if
C
  100   continue
C
        if(.not.KILROY) then
          call MASHED   ('DINANG')
        end if
C
      end if
C     !END
      call BYE ('DINANG')
C
      return
      end
