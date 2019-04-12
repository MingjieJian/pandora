      subroutine BAUTO
     $(A,N,K)
C
C     Rudolf Loeser, 1982 Oct 06
C---- Checks array, and computes antilogs, for OKRA.
C     (This is version 2 of BAUTO).
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, J, K, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external MESHED, ARROUT, ABORT, HI, BYE
C
C               A(N,K)
      dimension A(N,*)
C
      call HI ('BAUTO')
C     !BEG
      do 102 J = 1,K
        do 101 I = 1,N
          if(A(I,J).gt.ZLNLARG) then
            call MESHED ('BAUTO', 1)
            write (LUEO,100) I,J
  100       format(' ','Interpolated log(Input P.R.D. Jnu) is bad at '
     $                 'I =',I5,', J =',I5)
            call ARROUT (LUEO, A, N, K, 'log(JNU)')
            call ABORT
          end if
  101   continue
  102 continue
C
      do 104 J = 1,K
        do 103 I = 1,N
          A(I,J) = exp(A(I,J))
  103   continue
  104 continue
C     !END
      call BYE ('BAUTO')
C
      return
      end
