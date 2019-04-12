      subroutine GIGGLE
     $(X,IX,W,RHO,YBR,KRJ,KBDSE,IU,IL,TITLE,KPRNT)
C
C     Rudolf Loeser, 1978 Dec 03
C---- Debug printout of transition terms, when KPRNT > 0.
C
C     KBDSE =1 for b-ratio calculations,
C            2 for statistical equilibrium equations.
C     KRJ   =1 for "RHO with YBR" input,
C            2 for "YBR only" input,
C            3 for "RHO only" input.
C
C     TITLE contains up to 40 characters of comment.
C     (This is version 4 of GIGGLE.)
C     !DASH
      save
C     !DASH
      real*8 RHO, W, X, YBR
      integer I, IL, IN, IS, ITERM, IU, IX, KBDSE, KPRNT, KRJ, LDINT,
     $        LDTYP, LUEO, MDTYP, MOX, N, NL
      logical ANY, BD, PRINT, PRNTI, SE
      character TITLE*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
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
      equivalence (KZQ( 49),LDTYP)
      equivalence (KZQ( 48),LDINT)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LESZEK, TORGILS, MESHED, MASHED, MINNA, RENT, TENT,
     $         WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*)
C
C               RHO(N,NT), YBR(N,NT)
      dimension RHO(*),    YBR(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),ITERM )
C
      call HI ('GIGGLE')
C     !BEG
      call TORGILS    (KPRNT, PRINT)
C
      if(PRINT) then
        MDTYP = LDTYP
        if((MDTYP.lt.1).or.(MDTYP.gt.3)) then
          MDTYP = 1
        end if
C
        SE  = ((MDTYP.eq.1).and.(KBDSE.eq.2)).or.(MDTYP.eq.3)
        BD  = ((MDTYP.eq.2).and.(KBDSE.eq.1)).or.(MDTYP.eq.3)
        ANY = SE.or.BD
C
        if(ANY) then
C         (Get W allotment)
          call LESZEK   (IN, IS, MOX, 'GIGGLE')
C
          call MESHED   ('GIGGLE', 2)
          write (LUEO,100) TITLE,KBDSE,KRJ,LDTYP,LDINT
  100     format(' ','Debug printout of transition terms; controlled ',
     $               'by option ARHODMP.'//
     $           ' ',A40,5X,'KBDSE',I2,5X,'KRJ',I2,5X,'LDTYP',I2,
     $               5X,'LDINT',I6)
C
          do 101 I = 1,N
            call MINNA  (PRINT, I,LDINT, PRNTI)
            if(PRNTI) then
              call RENT (I, NL, X, IX, RHO, YBR, KRJ,         W(ITERM))
              call TENT (I, NL, X, IX,      YBR, KRJ, IU, IL, W(ITERM))
            end if
  101     continue
C
          call MASHED   ('GIGGLE')
C
C         (Give back W allotment)
          call WGIVE    (W, 'GIGGLE')
        end if
      end if
C     !END
      call BYE ('GIGGLE')
C
      return
      end
