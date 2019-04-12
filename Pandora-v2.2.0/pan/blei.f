      subroutine BLEI
     $(N,Z,FOLD,FNEW,FNED,FNSM,FINL,W,IW,SMGMMA,LABEL,DUMP)
C
C     Rudolf Loeser, 1999 Mar 19
C---- Computes a new, final function value, for Diffusion.
C     For "gamma-final" only (see also BILE).
C     !DASH
      save
C     !DASH
      real*8 F04, F10, FINL, FNED, FNEW, FNSM, FOLD, FOUR, HALF, OLD,
     $       OLDL, ONE, SIX, TWO, W, WGAM, XGAM, Z, ZERO
      integer I, IFO, IIMG, IN, INDX, IS, IW, IWS, JN, KMSS, KODE, LUEO,
     $        MODE, MOX, MUX, N, NERM, jummy
      logical DUMP, SMGMMA, lummy
      character BLANK*1, CD*16, CN*16, CO*16, LABEL*(*), MARK*1, STAR*1,
     $          TITLE*100, TYPE*3
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 5),FOUR  )
      equivalence (DLIT( 7),SIX   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external IMMAKE, FOMAKE, EDITH, SMOOTH, LINER, NOTICE, MOVE1,
     $         SHIM, IGIVE, WGIVE, HI, BYE
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
      data F10,F04 /1.D-10, 1.D-4/
C
      call HI ('BLEI')
C     !BEG
C     (Get W & IW allotments)
      call FOMAKE   (IN, IS , MOX, 'BLEI')
      call IMMAKE   (JN, IWS, MUX, 'BLEI')
C
      KMSS = 0
      if(DUMP) then
        KMSS = 1
      end if
      TITLE = LABEL//'-new (diffusion)'
      call MOVE1    (FNEW, N, FNED)
      call EDITH    (FNED, N, ZERO, KODE, MODE, KMSS, TITLE, IW(IIMG),
     $               W(IFO), jummy, NERM, lummy)
      call MOVE1    (FNED, N, FNSM)
      if(SMGMMA) then
        call SMOOTH (Z, FNSM, N, TYPE, TITLE, INDX, W, IW, jummy, lummy)
      end if
C
      do 100 I = 1,N
        OLD  = FOLD(I)
        OLDL = log10(OLD)
        XGAM = -(FOUR+OLDL)/SIX
        if((OLD.gt.F10).and.(OLD.lt.F04)) then
          WGAM = (ONE-XGAM)/TWO+XGAM*OLD
        else
          if(OLD.ge.F04) then
            WGAM = HALF
          else
            WGAM = OLD
          end if
        end if
        FINL(I) = (ONE-WGAM)*OLD+WGAM*FNSM(I)
  100 continue
C     !EJECT
      if(DUMP) then
        call LINER      (5, LUEO)
C
        if(SMGMMA) then
          write (LUEO,101) LABEL
  101     format(' ','===== Calculation of ',A,'-final.'//
     $           ' ',20X,'Z',13X,'new',10X,'edited',8X,'smoothed',
     $               12X,'final',8X,'compared',13X,'old')
          call LINER    (1, LUEO)
          do 103 I = 1,N
            call NOTICE (16, FOLD(I), FINL(I), CO, CN, CD)
            if(FNSM(I).eq.FNEW(I)) then
              MARK = BLANK
            else
              MARK = STAR
            end if
            write (LUEO,102) I,Z(I),FNEW(I),FNED(I),FNSM(I),MARK,
     $                       CN,CD,CO
  102       format(' ',I5,1P4E16.8,A1,3A16)
            call SHIM   (I, 5, LUEO)
  103     continue
C
        else
C
          write (LUEO,104) LABEL
  104     format(' ','===== Calculation of ',A,'-final.'//
     $           ' ',20X,'Z',13X,'new',10X,'edited',11X,'final',8X,
     $              'compared',13X,'old')
          call LINER    (1, LUEO)
          do 106 I = 1,N
            call NOTICE (16,FOLD(I),FINL(I),CO,CN,CD)
            write (LUEO,105) I,Z(I),FNEW(I),FNED(I),CN,CD,CO
  105       format(' ',I5,1P3E16.8,3A16)
            call SHIM   (I, 5, LUEO)
  106     continue
        end if
C
        call LINER      (1, LUEO)
        write (LUEO,107) LABEL
  107   format(' ','===== End of calculation of ',A,'-final.')
        call LINER      (5, LUEO)
      end if
C
C     (Give back W & IW allotments)
      call WGIVE        (W , 'BLEI')
      call IGIVE        (IW, 'BLEI')
C     !END
      call BYE ('BLEI')
C
      return
      end
