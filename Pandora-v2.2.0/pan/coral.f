      subroutine CORAL
     $(N,TE,XNE,Z,PFT,W,IW,NO)
C
C     Rudolf Loeser, 1982 Jun 29
C---- Sets up table of ratios of depth-varying partition functions.
C     (This is version 6 of CORAL.)
C     !DASH
      save
C     !DASH
      real*8 PFT, TE, W, XNE, Z
      integer IN, INDEX, IPE, IPFE, IPFO, IPFP, IPNT, IRAT, IS, ITHET,
     $        IVEC, IW, IWS, IZLOG, JN, MOX, MUX, N, NO, NT
C     !DASH
      external BIKINI, LORCA, DICTATE, LAGOON, ESPINA, DINGO, BREMEN,
     $         THETA, POLYP, WGIVE, IGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               TE(N), XNE(N), Z(N), PFT(N,NMT)
      dimension TE(*), XNE(*), Z(*), PFT(*)
C
      dimension IN(8)
      equivalence
     $(IN( 1),ITHET ),(IN( 2),IPE   ),(IN( 3),IZLOG ),(IN( 4),IPFO  ),
     $(IN( 5),IPFE  ),(IN( 6),IPFP  ),(IN( 7),IRAT  ),(IN( 8),IVEC  )
C
      dimension JN(2)
      equivalence
     $(JN( 1),INDEX ),(JN( 2),IPNT  )
C     !EJECT
C
      call HI ('CORAL')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call BIKINI  (IN,IS ,MOX,'CORAL')
      call BREMEN  (JN,IWS,MUX,'CORAL')
C
C---- Compute reciprocal temperature
      call THETA   (TE,W(ITHET),N)
C---- Compute electron pressure
      call POLYP   (TE,XNE,W(IPE),N)
C---- Compute index table and table count
      call DICTATE (IW(INDEX),NT,IW(IPNT))
C---- Compute Partition Functions
      call LAGOON  (N,NT,IW(INDEX),W(ITHET),W(IPE),W(IPFO),W(IPFE),
     $              W(IVEC))
C---- Compute ratios
      call ESPINA  (N,NT,IW(INDEX),W(IPFE),PFT)
C---- Print
      call LORCA   (NO,N,NT,IW(INDEX),W(ITHET),W(IPE),W(IPFO),W(IPFE),
     $              PFT)
C---- Plot
      call DINGO   (NO,N,PFT,Z,W(IRAT),IW(IPNT),W(IPFP),W(IZLOG))
C
C     (Give back W & IW allotments)
      call WGIVE   (W ,'CORAL')
      call IGIVE   (IW,'CORAL')
C     !END
      call BYE ('CORAL')
C
      return
      end
