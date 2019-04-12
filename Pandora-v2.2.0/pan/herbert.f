      subroutine HERBERT
     $(X,W,IW,XLCOA,XLCOB,NCB,WAVES,JIND,KIND,NIND,MIND,NWV,KODE)
C
C     Rudolf Loeser, 1987 Nov 17
C---- Sets up a complete table of CO wavelengths for the specified
C     spectrum ranges, with tables of descriptive indices.
C
C     WAVES, JIND, KIND, NIND and MIND must each provide room for at
C     least LCOW*(2*NCL-1)+2*NCB items [LCOW is the number of CO lines
C     in use (q.v. HEGRO)]; their final lengths will = NWV.
C
C     JIND, KIND, NIND and MIND will be set up only if input KODE=1,
C     but not if KODE=0.
C     !DASH
      save
C     !DASH
      real*8 W, WAVES, X, XLCOA, XLCOB
      integer IJB, IKB, IMB, IN, IS, ISCOL, IW, IWS, IWVB, JIND, JJCOL,
     $        JJTE, JJV, JN, KIND, KODE, MIND, MOX, MUX, NCB, NCL, NDW,
     $        NIND, NWV
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(51),NCL)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 12),JJV  )
      equivalence (IZOQ(183),JJCOL)
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
      equivalence (KZQ(  1),NDW  )
C     !DASH
C     !EJECT
      external INAUS, GILEAD, STOMP, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C     (See note above for upper limit of NWV.)
C
C               JIND(NWV), KIND(NWV), NIND(NWV), MIND(NWV), WAVES(NWV),
      dimension JIND(*),   KIND(*),   NIND(*),   MIND(NWV), WAVES(*),
C
C               XLCOA(NCB), XLCOB(NCB)
     $          XLCOA(*),   XLCOB(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IWVB  ),(IN( 2),ISCOL )
C
      dimension JN(3)
      equivalence
     $(JN( 1),IJB   ),(JN( 2),IKB   ),(JN( 3),IMB   )
C
      call HI ('HERBERT')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call INAUS   (IN, IS,  MOX, 'HERBERT')
      call GILEAD  (JN, IWS, MUX, 'HERBERT')
C
      NWV = 0
      call STOMP   (XLCOA, XLCOB, NCB, X(JJCOL), NCL, NDW, X(JJTE),
     $              X(JJV), WAVES, JIND, KIND, NIND, MIND, NWV,
     $              W(ISCOL), W(IWVB), IW(IJB), IW(IKB), IW(IMB),
     $              KODE, IW)
C
C     (Give back W & IW allotments)
      call WGIVE   (W,  'HERBERT')
      call IGIVE   (IW, 'HERBERT')
C     !END
      call BYE ('HERBERT')
C
      return
      end
