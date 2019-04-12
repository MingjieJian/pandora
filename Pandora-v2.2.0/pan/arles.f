      subroutine ARLES
     $(A,N,K,IU,IL)
C
C     Rudolf Loeser, 1982 Oct 06
C---- Checks the array and computes logs, for OKRA.
C     (This is version 2 of ARLES.)
C     !DASH
      save
C     !DASH
      real*8 A, ASAVED, ZERO
      integer I, IL, IU, J, K, LUEO, N
      logical KILROY
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
C     !EJECT
      external  MESHED, MASHED, LINER, HI, BYE
      intrinsic max, abs
C
C               A(N,K)
      dimension A(N,*)
C
      call HI ('ARLES')
C     !BEG
      KILROY = .true.
C
      do 103 J = 1,K
        do 102 I = 1,N
C
          if(A(I,J).le.ZERO) then
            ASAVED = A(I,J)
            A(I,J) = max(ZZSMALL, abs(A(I,J)))
            if(KILROY) then
              KILROY = .false.
              call MESHED ('ARLES', 3)
              write (LUEO,100) IU,IL
  100         format(' ','Bad input value(s) of JNU for transition (',
     $                    I2,'/',I2,')')
              call LINER  (1, LUEO)
            end if
            write (LUEO,101) I,J,ASAVED,A(I,J)
  101       format(' ','At i =',I5,', j =',I5,', changed from',1PE16.8,
     $                 ' to',E16.8)
          end if
C
          A(I,J) = log(A(I,J))
C
  102   continue
  103 continue
C
      if(.not.KILROY) then
        call MASHED       ('ARLES')
      end if
C     !END
      call BYE ('ARLES')
C
      return
      end
