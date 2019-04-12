      subroutine SMOG
     $(A,N,IS,IE,W,KMSS,TITLE)
C
C     Rudolf Loeser, 1977 Feb 03
C---- Supervises the smoothing of A.
C     !DASH
      save
C     !DASH
      real*8 A, W, ZERO
      integer IE, IS, JE, JS, KMSS, LUEO, N
      character TITLE*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  MESHED, CLARICE, VECOUT, MASHED, HI, BYE
      intrinsic max, min
C
C               A(N)
      dimension A(*)
C     !EJECT
C
      call HI ('SMOG')
C     !BEG
      if(W.gt.ZERO) then
        JS = max(1,IS)
        JE = min(IE,N)
        if(JE.le.0) then
          JE = N
        end if
C
        if(KMSS.gt.0) then
          call MESHED ('SMOG', 3)
          write (LUEO,100) W,JS,JE
  100     format(' ','Smoothing parameters: W =',1PE23.4,', JS =',I8,
     $               ', JE =',I8)
          call VECOUT (LUEO, A, N, ('Old '//TITLE))
        end if
C
        call CLARICE  (A, JS, JE, W)
C
        if(KMSS.gt.0) then
          call VECOUT (LUEO, A, N, ('New, smoothed '//TITLE))
          call MASHED ('SMOG')
        end if
C
      end if
C     !END
      call BYE ('SMOG')
C
      return
      end
