      subroutine PERION
     $(W,IW,SWAVE,SLTIT,NSH)
C
C     Rudolf Loeser, 2002 Aug 22
C---- Sorts temporary supplementary header tables.
C     !DASH
      save
C     !DASH
      real*8 SLTIT, SWAVE, W
      integer IN, IPNT, IS, IVEC, IW, IWS, JN, MOX, MUX, NSH
C     !DASH
      external NOPIER, NEPORI, SORT, ORDERD, WGIVE, IGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               SWAVE(Konwal), SLTIT(Konwal)
      dimension SWAVE(*),      SLTIT(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IVEC  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IPNT  )
C
      call HI ('PERION')
C     !BEG
      if(NSH.gt.1) then
        call NOPIER (IN, IS , MOX, 'PERION')
        call NEPORI (JN, IWS, MUX, 'PERION')
C
        call SORT   (SWAVE, NSH, IW(IPNT),
     $               'Temporary supplementary headers')
        call ORDERD (SLTIT, IW(IPNT), NSH, W(IVEC))
C
        call WGIVE  (W , 'PERION')
        call IGIVE  (IW, 'PERION')
      end if
C     !END
      call BYE ('PERION')
C
      return
      end
