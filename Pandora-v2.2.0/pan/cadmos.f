      subroutine CADMOS
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1983 Jun 29
C---- Supervises processing of Line Opacities data.
C     (This is version 4 of CADMOS.)
C     !DASH
      save
C     !DASH
      real*8 W, WAVE, X
      integer IALB, IN, INDEX, IS, IW, IX, JJTE, KININ, KINMX, MOX, N
      logical AVERAG, COMPOS, LINEOP, STATST
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  7),JJTE )
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
      equivalence (KZQ( 28),KINMX)
      equivalence (KZQ( 29),KININ)
C
C---- KNTKUPL     as of 1993 Sep 15
      integer     KNTKU
      parameter   (KNTKU=5)
C     Control parameter for "Line" opacity plots.
C     (Used in CADMOS, MAYA, SILAS.)
C     .
C     !DASH
      external EGRON, SABLE, LET, NARSAH, DEVI, KALI, HIRAM, WGIVE,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension INDEX(KNTKU)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IALB   )
C
      data WAVE /5.D3/
C
      call HI ('CADMOS')
C     !BEG
C     (Get W allotment)
      call EGRON    (IN,IS,MOX,'CADMOS')
C
      call SABLE    (STATST,COMPOS,AVERAG,LINEOP)
      if(LINEOP) then
C----   Compute sample values of albedo
        call LET    (X,WAVE,W(IALB))
C----   Set up plot selectors
        call NARSAH (KINMX,KININ,X(JJTE),N,INDEX,KNTKU)
      end if
C
      if(STATST) then
C----   Statistical line opacity
        call DEVI   (X,   W,   INDEX,KNTKU,W(IALB),WAVE)
      end if
      if(COMPOS) then
C----   Composite line opacity
        call KALI   (X,IX,W,IW,INDEX,KNTKU,W(IALB),WAVE)
      end if
      if(AVERAG) then
C-----  "Averaged" line opacity
        call HIRAM  (X,   W,   INDEX,KNTKU,W(IALB),WAVE)
      end if
C
C     (Give back W allotment)
      call WGIVE    (W,'CADMOS')
C     !END
      call BYE ('CADMOS')
C
      return
      end
