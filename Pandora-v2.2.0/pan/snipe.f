      subroutine SNIPE
     $(NO,NW,SPHERE,XMULT,LTYPE,XLTIT,WAVE,WTAB,FHZ,FA,TB,TF,SF)
C
C     Rudolf Loeser, 1986 Feb 21
C---- Prints continuum flux.
C     !DASH
      save
C     !DASH
      real*8 FA, FHZ, SF, SHL, TB, TF, WAVE, WTAB, XLTIT, XMULT
      integer I, LTYPE, NO, NW
      logical SPHERE
      character BLANK*1, HEADER*40, LABEL*29, MULT*6, SAVE*5, SHLR*9,
     $          qummy*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external KAKAPO, PLOP, MONGOL, ALARSK, SHIM, HI, BYE
C
C               TB(Nmkuse), LTYPE(Nmkuse), XLTIT(Nmkuse), WAVE(Nmkuse),
      dimension TB(*),      LTYPE(*),      XLTIT(*),      WAVE(*),
C
C               FHZ(Nmkuse), XMULT(Nmkuse), WTAB(Nmkuse), SF(Nmkuse),
     $          FHZ(*),      XMULT(*),      WTAB(*),      SF(*),
C
C               TF(Nmkuse), FA(Nmkuse)
     $          TF(*),      FA(*)
C
      call HI ('SNIPE')
C     !BEG
      if(NO.gt.0) then
        do 102 I = 1,NW
          call KAKAPO   (I, WAVE(I), WTAB(I), LABEL, SAVE)
          call PLOP     (XMULT(I), LTYPE(I), MULT)
          call MONGOL   (XLTIT(I), HEADER, qummy)
C
          if(SPHERE) then
            call ALARSK (SPHERE, SF(I), TF(I), SHL)
            write (SHLR,100) SHL
  100       format(1X,F8.5)
          else
            SHLR = BLANK
          end if
C
          write (NO,101) I,LABEL,MULT,FHZ(I),FA(I),TB(I),SHLR,
     $                   HEADER(1:39)
  101     format(' ',I7,A29,' ',A6,' ',1P3E12.4,A9,' ',A37)
          call SHIM     (I, 5, NO)
  102   continue
      end if
C     !END
      call BYE ('SNIPE')
C
      return
      end
