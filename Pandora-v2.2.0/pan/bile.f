      subroutine BILE
     $(N,Z,FOLD,FNEW,FNED,FNSM,FINL, W,IW, LABEL,LU)
C
C     Rudolf Loeser, 1999 Jan 13
C---- Computes a new, final function value, for Diffusion.
C     Now for "alpha-final" only (see also BLEI).
C     (This is version 3 of BILE.)
C     !DASH
      save
C     !DASH
      real*8 AOWXP, FINL, FNED, FNEW, FNSM, FOLD, W, Z, ZERO
      integer I, IFO, IIMG, IN, INDX, IS, IW, IWS, JN, KMSS, KODE, LU,
     $        MODE, MOX, MUX, N, NERM, jummy
      logical lummy
      character BLANK*1, CD*16, CN*16, CO*16, LABEL*(*), MARK*1, STAR*1,
     $          TITLE*100, TYPE*3
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
      equivalence (RZQ(156),AOWXP)
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
      equivalence (SYMBS(45),STAR  )
C     !DASH
C     !EJECT
      external  IGIVE, MOVE1, EDITH, SMOOTH, LINER, SHIM, NOTICE, NAHE,
     $          IMMAKE, FOMAKE, WGIVE, HI, BYE
      intrinsic min, max
C
      dimension W(*), IW(*)
C
C               Z(N), FOLD(N), FNEW(N), FNED(N), FNSM(N), FINL(N)
      dimension Z(*), FOLD(*), FNEW(*), FNED(*), FNSM(*), FINL(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IFO   )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIMG  )
C
      data TYPE,KODE,MODE,NERM,INDX /'lin', 2, 2, 0, 0/
C
      call HI ('BILE')
C     !BEG
C     (Get W & IW allotments)
      call FOMAKE     (IN, IS , MOX, 'BILE')
      call IMMAKE     (JN, IWS, MUX, 'BILE')
C
      TITLE = LABEL//'-new (diffusion)'
C---- Editing . . . .
      call MOVE1      (FNEW, N, FNED)
      KMSS = max(min(LU,1),0)
      call EDITH      (FNED, N, ZERO, KODE, MODE, KMSS, TITLE,
     $                 IW(IIMG), W(IFO), jummy, NERM, lummy)
C---- . . . . Smmothing . . . .
      call MOVE1      (FNED, N, FNSM)
      call SMOOTH     (Z, FNSM, N, TYPE, TITLE, INDX, W, IW,
     $                 jummy, lummy)
C---- . . . . Weighting
      call NAHE       (N, FOLD, FNSM, FINL, AOWXP)
C     !EJECT
      if(LU.gt.0) then
C       Printing
        call LINER    (5, LU)
        write (LU,100) LABEL
  100   format(' ','===== Calculation of ',A,'-final.'//
     $         ' ',20X,'Z',13X,'new',10X,'edited',8X,'smoothed',
     $             12X,'final',8X,'compared',13X,'old')
        call LINER    (1, LU)
C
        do 102 I = 1,N
          call NOTICE (16, FOLD(I), FINL(I), CO, CN, CD)
          if(FNSM(I).eq.FNEW(I)) then
            MARK = BLANK
          else
            MARK = STAR
          end if
          write (LU,101) I,Z(I),FNEW(I),FNED(I),FNSM(I),MARK,CN,CD,CO
  101     format(' ',I5,1P4E16.8,A1,3A16)
          call SHIM   (I, 5, LU)
  102   continue
C
        call LINER    (1, LU)
        write (LU,103) AOWXP
  103   format(' ','Weight for "old" = [ 1 - ("old")**AOWXP ], ',
     $             'where AOWXP =',1PE11.4)
C
        call LINER    (1, LU)
        write (LU,104) LABEL
  104   format(' ','===== End of calculation of ',A,'-final.')
        call LINER    (5, LU)
      end if
C
C     (Give back W & IW allotments)
      call WGIVE      (W , 'BILE')
      call IGIVE      (IW, 'BILE')
C     !END
      call BYE ('BILE')
C
      return
      end
