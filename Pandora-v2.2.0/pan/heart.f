      subroutine HEART
     $(NAB,BANDL,BANDU,L,EMU,NW,LTYPE,WAVES,YHZ,YAN,BT,NCP,WAVESB,YHZB,
     $ YANB,BTB,NBT,IST,IET,NO,IJECT,LFB,WVNUMB)
C
C     Rudolf Loeser, 1983 Oct 27
C---- Computes band averages for Composite Line Opacity wavelengths
C     (TYPE 13).
C     !DASH
      save
C     !DASH
      real*8 BANDL, BANDU, BT, BTA, BTB, EMU, WAVES, WAVESB, WVNUMB,
     $       YAN, YANA, YANB, YHZ, YHZA, YHZB, ZERO
      integer I, IE, IET, IJECT, IS, IST, J, L, LFB, LTYPE, NAB, NB,
     $        NBT, NCP, NO, NTB, NW, NWB
      logical WAVENO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LUNG, LIVER, KIDNEY, SPLEEN, BOWEL, DUNGUS, HI, BYE
C
C               LTYPE(Numkon), YHZ(Numkon), WAVESB(NCP), WAVES(Numkon),
      dimension LTYPE(*),      YHZ(*),      WAVESB(*),   WAVES(*),
C
C               IST(NAB), IET(NAB), YHZB(NCP,L), YANB(NCP,L), NBT(NAB),
     $          IST(*),   IET(*),   YHZB(NCP,*), YANB(NCP,*), NBT(*),
C
C               BTB(NCP,L), WVNUMB(NCP), YAN(Numkon), BANDU(NAB),
     $          BTB(NCP,*), WVNUMB(*),   YAN(*),      BANDU(*),
C
C               BT(Numkon), BANDL(NAB), EMU(L)
     $          BT(*),      BANDL(*),   EMU(*)
C
      call HI ('HEART')
C     !BEG
      if((NAB.gt.0).and.(NO.gt.0)) then
C----   Cull out "Composite" wavelengths
        call LUNG       (NW, WAVES, LTYPE, L, NCP, WAVESB, YHZ, YAN,
     $                   BT, YHZB, YANB, BTB, NWB)
C     !EJECT
        if(NWB.gt.0) then
          call DUNGUS   (WAVESB, ZERO, NCP, WVNUMB)
C
C----     Find limiting indices for each band
          NTB = 0
          do 100 J = 1,NAB
            call KIDNEY (NCP, WAVESB, BANDL(J), BANDU(J), NBT(J),
     $                   IST(J), IET(J))
            NTB = NTB+NBT(J)
  100     continue
C
          if(NTB.gt.0) then
C----       Loop over all look angles
            do 102 I = 1,L
C
C----         Print header
              call LIVER      (NO, I, EMU, LFB, WAVENO, IJECT)
C
C----         Loop over all bands
              do 101 J = 1,NAB
                NB = NBT(J)
                if(NB.gt.0) then
                  IS = IST(J)
                  IE = IET(J)
C----             Compute averages
                  call SPLEEN (WAVESB, YHZB(1,I), YANB(1,I), BTB(1,I),
     $                         IS, NB, YHZA, YANA, BTA)
C----             Print averages
                  call BOWEL  (NO, J, NB, WAVESB(IS), WAVESB(IE),
     $                         WAVENO, WVNUMB(IE), WVNUMB(IS), YHZA,
     $                         YANA, BTA)
                end if
C
  101         continue
C
  102       continue
          end if
        end if
      end if
C     !END
      call BYE ('HEART')
C
      return
      end
