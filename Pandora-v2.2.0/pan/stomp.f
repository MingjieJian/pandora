      subroutine STOMP
     $(XLCOA,XLCOB,NCB,XCOL,NCL,NDW,TE,V,WAVES,JIND,KIND,NIND,MIND,
     $ NWV,SCL,WVB,J,K,M,KODE,IW)
C
C     Rudolf Loeser, 1987 Nov 16
C---- Makes a complete CO-lines continuum wavelengths table for all the
C     wavelength intervals (XLCOA,XLCOB), and establishes the
C     "dump" wavelength.
C     (This is version 3 of STOMP.)
C     !DASH
      save
C     !DASH
      real*8 SCL, TE, V, WAVES, WVB, XCOL, XLCOA, XLCOB, XLCOW, ZERO
      integer I, IW, J, JIND, K, KIND, KODE, LCOW, M, MIND, NB, NCB,
     $        NCL, NDW, NIND, NW, NWV
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(44),LCOW )
      equivalence (REST( 4),XLCOW)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ADELMA, MOVE1, MOVEI, WEIR, KADAR, ZEROI, HI, BYE
C
      dimension IW(*)
C
C     The upper limit on NWV and NB is LCOW*(2*NCL-1)+2*NCB, where
C     LCOW is the number of CO line cores included.
C
C               XCOL(NCL), TE(N), V(N), WAVES(NWV), JIND(NWV), WVB(NB),
      dimension XCOL(*),   TE(*), V(*), WAVES(*),   JIND(*),   WVB(*),
C
C               KIND(NWV), J(NB), K(NB), M(NB), XLCOA(NCB), XLCOB(NCB),
     $          KIND(*),   J(*),  K(*),  M(*),  XLCOA(*),   XLCOB(*),
C
C               SCL(NCL,2), NIND(NWV), MIND(NWV)
     $          SCL(*),     NIND(*),   MIND(*)
C     !EJECT
C
      call HI ('STOMP')
C     !BEG
      NWV   = 0
      XLCOW = ZERO
C
      if(LCOW.gt.0) then
        do 100 I = 1,NCB
C----     Make basic table (with descriptive indices)
          NB = 0
          call ADELMA      (XLCOA(I), XLCOB(I), KODE, WVB, J, K, M, NB,
     $                      IW)
          if(NB.gt.0) then
            if(NCL.eq.1) then
C----         This is it -- move stuff into proper slots
C             (Thus, XCOL(1)=0 is  P R E S U M E D  , so that only line
C             cores and band limits are entered into WAVES).
              call MOVE1   (WVB, NB, WAVES(NWV+1))
              if(KODE.eq.1) then
                call MOVEI (J, 1, NB, JIND(NWV+1), 1, NB)
                call MOVEI (K, 1, NB, KIND(NWV+1), 1, NB)
                call MOVEI (M, 1, NB, MIND(NWV+1), 1, NB)
                call ZEROI (NIND(NWV+1), 1, NB)
              end if
              NWV = NWV+NB
            else
C----         Make expanded set
              call WEIR    (WVB, J, K, M, NB, XCOL, NCL, NDW, TE, V,
     $                      SCL, WAVES(NWV+1), JIND(NWV+1),
     $                      KIND(NWV+1), NIND(NWV+1), MIND(NWV+1), NW,
     $                      KODE)
              NWV = NWV+NW
            end if
          end if
  100   continue
C
C----   Set up dump wavelength
        call KADAR         (WAVES, NWV)
      end if
C     !END
      call BYE ('STOMP')
C
      return
      end
