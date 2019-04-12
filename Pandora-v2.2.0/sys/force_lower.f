      subroutine FORCE_LOWER
     $(STRIN, STROUT)
C     Rudolf Loeser, 1995 Nov 06
C---- Sets STROUT = STRIN, but with all upper-case characters
C     of STRIN replaced with their lower-case equivalents.
C     (The user must make sure that STROUT is long enough.)
C     !DASH
      save
C     !DASH
      integer I, J, JA, JD, JZ, KK, L, LA, LD, LZ, NA, NZ
      logical FAST, KILROY
      character BLANK*1, LO*1, STRIN*(*), STROUT*(*), UP*1
C     !DASH
      intrinsic char, ichar, len
C
      dimension UP(26), LO(26)
C
      data  UP /'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
     $          'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
     $          'U', 'V', 'W', 'X', 'Y', 'Z'/
      data  LO /'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
     $          'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
     $          'u', 'v', 'w', 'x', 'y', 'z'/
      data BLANK /' '/
      data KILROY /.true./
C     !EJECT
C
C     !BEG
      if(KILROY) then
        KILROY = .false.
        JA = ichar(LO( 1))
        JZ = ichar(LO(26))
        JD = JZ-JA
        LA = ichar(UP( 1))
        LZ = ichar(UP(26))
        LD = LZ-LA
        NA = JA-LA
        NZ = JZ-LZ
        FAST = (JD.eq.25).and.(LD.eq.25).and.(NA.eq.NZ)
      end if
C
      L = len(STRIN)
      do 101 J=1,L
        KK = ichar(STRIN(J:J))
        if((LA.le.KK).and.(KK.le.LZ)) then
          if(FAST) then
            STROUT(J:J) = char(KK+NA)
            goto 101
          else
            do 100 I=1,26
              if(STRIN(J:J).eq.UP(I)) then
                STROUT(J:J) = LO(I)
                goto 101
              end if
  100       continue
          end if
        end if
        STROUT(J:J) = STRIN(J:J)
  101 continue
C     !END
C
      return
      end
