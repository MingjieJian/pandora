      subroutine DONTAS
     $(DETAIL,LDL,INDL,K,LIST,LNGTH)
C
C     Rudolf Loeser, 1991 Aug 08
C---- Sets up the list of profile wavelengths for dI/dh output.
C     !DASH
      save
C     !DASH
      integer I, INDL, K, LDL, LIST, LNGTH
      logical DETAIL
C     !DASH
      external MOVEI, HI, BYE
C
C               LIST(LNGTH), INDL(LDLMX)
      dimension LIST(*),     INDL(*)
C
      call HI ('DONTAS')
C     !BEG
      if(DETAIL) then
        do 100 I = 1,K
          LIST(I) = I
  100   continue
        LNGTH = K
      else
        call MOVEI (INDL, 1, LDL, LIST, 1, LDL)
        LNGTH = LDL
      end if
C     !END
      call BYE ('DONTAS')
C
      return
      end
