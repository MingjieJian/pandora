      subroutine RADULA
     $(NO,NW,WAVES,WTAB,KODE,MUX,YY,OPACF,EMINT,YSTAR,BRIGHT,LTYPA,
     $ XLTIT,MYX,AVCON)
C
C     Rudolf Loeser, 1098 Feb 21
C---- Prints continuum intensity.
C     !DASH
      save
C     !DASH
      real*8 AVCON, BRIGHT, EMINT, OPACF, WAVES, WTAB, XLTIT, YSTAR, YY,
     $       ZERO
      integer I, KODE, LTYPA, MUX, MYX, NO, NW
      logical KILROY
      character FK*13, FLAG*1, HEADER*40, LABEL*29, LINE*127, MULT*6,
     $          SAVE*5, qummy*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external  KAKAPO, TISRIT, NIT, PLOP, MONGOL, SHIM, LINER, SMUDGE,
     $          HI, BYE
C
C               WAVES(Nmkuse), KODE(Nmkuse), LTYPA(Nmkuse), YY(Nmkuse),
      dimension WAVES(*),      KODE(*),      LTYPA(*),      YY(*),
C
C               EMINT(Nmkuse), MYX(Nmkuse), YSTAR(Nmkuse), MUX(Nmkuse),
     $          EMINT(*),      MYX(*),      YSTAR(*),      MUX(*),
C
C               XLTIT(Nmkuse), OPACF(Nmkuse), BRIGHT(Nmkuse),
     $          XLTIT(*),      OPACF(*),      BRIGHT(*),
C
C               WTAB(Nmkuse)
     $          WTAB(*)
C
      call HI ('RADULA')
C     !BEG
      if(NO.gt.0) then
        KILROY = .true.
C
        do 101 I = 1,NW
          call KAKAPO (I, WAVES(I), WTAB(I), LABEL, SAVE)
          call TISRIT (MUX(I), YY(I), MYX(I), FK)
          call NIT    (KILROY, KODE(I), FLAG)
          call PLOP   (OPACF(I), LTYPA(I), MULT)
          call MONGOL (XLTIT(I), HEADER, qummy)
C
          write (NO,100) I,LABEL,FK,FLAG,MULT,EMINT(I),YSTAR(I),
     $                   BRIGHT(I),HEADER(1:35)
  100     format(' ',I7,A29,' ',A13,A1,' ',A6,1PE12.4,2E10.2,' ! ',A35)
          call SHIM   (I, 5, NO)
  101   continue
C
        if(AVCON.ne.ZERO) then
          call LINER  (1, NO)
          write (NO,102) AVCON
  102     format(' ',38X,'Average intensity',1PE12.4)
        end if
C
        call SMUDGE   (NO, 48, LINE)
      end if
C     !END
      call BYE ('RADULA')
C
      return
      end
