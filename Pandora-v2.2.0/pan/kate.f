      subroutine KATE
     $(INDEX,KODE,KIJ,NL,IU,IL, IN)
C
C     Rudolf Loeser, 1980 Oct 31
C---- Determines whether or not the current transition
C     should be printed, for SCRIBE.
C
C     Input values of INDEX = 'IJ', 'UL', or 'NT' are meaningful.
C
C     Returns with IN .gt. 0 if yes, .eq. 0 if no.
C     If IN .gt. 0, then the value of IN is the
C     actual column index of the data for this transition.
C
C     KODE and KIJ are needed only when INDEX = 'NT'.
C     !DASH
      save
C     !DASH
      integer IL, IN, IU, K, KIJ, KODE, NL
      logical OK, USIT
      character INDEX*2
C     !DASH
      external  MARCH, INDXNT, HI, BYE
      intrinsic max
C
C               KIJ(NL,NL)
      dimension KIJ(NL,*)
C
      call HI ('KATE')
C     !BEG
      if(INDEX.eq.'NT') then
        K = KIJ(IU,IL)
C
        USIT = ((KODE.eq.3).or.(KODE.eq.K))
        if(((K.eq.1).or.(K.eq.2)).and.USIT) then
          call INDXNT (IU, IL, OK, IN)
          if(.not.OK) then
            IN = 0
          end if
        else
          IN = 0
        end if
C
      else
        call MARCH    (IU, IL, INDEX, 'KATE', IN)
      end if
C     !END
      call BYE ('KATE')
C
      return
      end
