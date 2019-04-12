      subroutine CLARICE
     $(A,IS,IE,W)
C
C     Rudolf Loeser, 1977 Feb 02
C---- Logarithmic smoothing routine.
C     The smoothed portion of the array A(i), IS .le. i .le. IE,
C     replaces the original values.
C     !DASH
      save
C     !DASH
      real*8 A, AC, AM, AP, HALF, RM, RP, W, ZERO
      integer I, IE, IEM, IS, ISP, KLOG
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external WEIGHT, HI, BYE
C
      dimension A(*)
C
      data KLOG /1/
C
      call HI ('CLARICE')
C     !BEG
      if((W.gt.ZERO).and.(IE.gt.IS)) then
        AC = A(IS)
        AP = A(IS+1)
        call WEIGHT     (AC,AP,W   ,KLOG,A(IS))
C
        ISP = IS+1
        IEM = IE-1
        if(IEM.ge.ISP) then
          do 100 I = ISP,IEM
            AM = AC
            AC = AP
            AP = A(I+1)
            call WEIGHT (AC,AM,W   ,KLOG,RM   )
            call WEIGHT (AC,AP,W   ,KLOG,RP   )
            call WEIGHT (RM,RP,HALF,KLOG,A(I ))
  100     continue
        end if
C
        AM = AC
        AC = A(IE)
        call WEIGHT     (AC,AM,W   ,KLOG,A(IE))
      end if
C     !END
      call BYE ('CLARICE')
C
      return
      end
