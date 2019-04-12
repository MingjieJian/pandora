      subroutine IRMAK
     $(NO,N,Z,TE,COOL,HEAT,TOTL,IQCCI,COOLI,HEATI,TOTLI)
C
C     Rudolf Loeser, 1986 Apr 21
C---- Prints summary of cooling/heating rates.
C     !DASH
      save
C     !DASH
      real*8 COOL, COOLI, D, HEAT, HEATI, T, TE, TI, TOTL, TOTLI, Z,
     $       ZERO
      integer I, IQCCI, N, NO
      logical BOTH
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  ABJECT, LINER, SHIM, DIVIDE, HI, BYE
      intrinsic abs,max
C
C               HEATI(N), TE(N), COOL(N), HEAT(N), TOTLI(N), COOLI(N),
      dimension HEATI(*), TE(*), COOL(*), HEAT(*), TOTLI(*), COOLI(*),
C
C               Z(N), TOTL(N)
     $          Z(*), TOTL(*)
C     !EJECT
C
      call HI ('IRMAK')
C     !BEG
      if(NO.gt.0) then
        BOTH = IQCCI.gt.0
C
        call ABJECT       (NO)
        write (NO,100)
  100   format(' ','Summary of computed cooling/heating rates.'//
     $         ' ','("Total" = (COOL - HEAT) / ',
     $             'max(abs(COOL), abs(HEAT)) .)')
        call LINER        (2,NO)
        if(BOTH) then
          write (NO,101)
  101     format(' ',85X,2(5X,'Integrated')/
     $           ' ',25X,2(23X,'Cooling',8X,'Heating')/
     $           ' ',19X,'Z',13X,'TE',2(16X,'Rate',11X,'Rate',
     $               3X,'"Total"'))
        else
          write (NO,102)
  102     format(' ',48X,'Cooling',8X,'Heating'/
     $           ' ',19X,'Z',13X,'TE',16X,'Rate',11X,'Rate',
     $               3X,'"Total"')
        end if
        call LINER        (1,NO)
C
        do 104 I = 1,N
          if(TOTL(I).eq.ZERO) then
            T = ZERO
          else
            D = max(abs(COOL(I)),abs(HEAT(I)))
            call DIVIDE   (TOTL(I),D,T )
          end if
          if(BOTH) then
            if(TOTLI(I).eq.ZERO) then
              TI = ZERO
            else
              D = max(abs(COOLI(I)),abs(HEATI(I)))
              call DIVIDE (TOTLI(I),D,TI)
            end if
            write (NO,103) I,Z(I),TE(I),COOL(I),HEAT(I),T,
     $                     COOLI(I),HEATI(I),TI
          else
            write (NO,103) I,Z(I),TE(I),COOL(I),HEAT(I),T
          end if
  103     format(' ',I5,1P2E15.7,5X,2E15.7,E10.2,:,5X,2E15.7,E10.2)
          call SHIM       (I,5,NO)
  104   continue
C
      end if
C     !END
      call BYE ('IRMAK')
C
      return
      end
