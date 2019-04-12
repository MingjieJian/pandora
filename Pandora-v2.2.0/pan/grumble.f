      subroutine GRUMBLE
     $(KIND,ION,THETA,PE,CHI,G0,SIGMA)
C
C     Rudolf Loeser, 2001 Jun 28
C---- Selects the appropriate Hamburg routine, and returns the
C     results for ion ION (whose index is KIND), at the given depth.
C     !DASH
      save
C     !DASH
      real*8 CHI, G0, PE, SIGMA, THETA
      integer ION, KIND
C     !DASH
      external PARTAL, PARTAR, PARTMN, PARTBA, PARTBE, PARTC, PARTS,
     $         PARTCA, PARTCL, PARTCO, PARTCR, PARTCU, PARTF, PARTFE,
     $         PARTGA, PARTZR, PARTHE, PARTK, PARTKR, PARTLI, PARTMG,
     $         PARTN, PARTNA, PARTNB, PARTNE, PARTNI, PARTO, PARTP,
     $         PARTSC, PARTSI, PARTSR, PARTTI, PARTV, PARTY, PARTZN,
     $         PARTB, PARTH, HI, BYE
C
      call HI ('GRUMBLE')
C     !BEG
      goto (101,102,103,104,105,106,107,108,109,110,111,112,113,114,
     $      115,116,117,118,119,120,121,122,123,124,125,126,127,128,
     $      129,130,131,132,133,134,135,136,137), KIND
C     !EJECT
  101 continue
        call PARTAL  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  102 continue
        call PARTAR  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  103 continue
        call PARTB   (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  104 continue
        call PARTBA  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  105 continue
        call PARTBE  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  106 continue
        call PARTC   (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  107 continue
        call PARTCA  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  108 continue
        call PARTCL  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  109 continue
        call PARTCO  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  110 continue
        call PARTCR  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  111 continue
        call PARTCU  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  112 continue
        call PARTF   (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  113 continue
        call PARTFE  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  114 continue
        call PARTGA  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  115 continue
        call PARTH   (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  116 continue
        call PARTHE  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  117 continue
        call PARTK   (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  118 continue
        call PARTKR  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  119 continue
        call PARTLI  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  120 continue
        call PARTMG  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  121 continue
        call PARTMN  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  122 continue
        call PARTN   (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  123 continue
        call PARTNA  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  124 continue
        call PARTNB  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  125 continue
        call PARTNE  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  126 continue
        call PARTNI  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  127 continue
        call PARTO   (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  128 continue
        call PARTP   (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  129 continue
        call PARTS   (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  130 continue
        call PARTSC  (ION,THETA,PE,CHI,G0,SIGMA)
       goto 100
  131 continue
        call PARTSI  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  132 continue
        call PARTSR  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  133 continue
        call PARTTI  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  134 continue
        call PARTV   (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  135 continue
        call PARTY   (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  136 continue
        call PARTZN  (ION,THETA,PE,CHI,G0,SIGMA)
        goto 100
  137 continue
        call PARTZR  (ION,THETA,PE,CHI,G0,SIGMA)
  100 continue
C     !END
      call BYE ('GRUMBLE')
C
      return
      end
