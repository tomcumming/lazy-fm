module FM.Roles where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..))
import FM.Attr (Attr(..))
import FM.Position as Pos
import FM.StandardsGroup as SG

type Role =
  { name :: String
  , group :: SG.StandardsGroup
  , positions :: Pos.Positions
  , primary :: Set.Set Attr
  , secondary :: Set.Set Attr
  }

zipRoleAttributes
  :: Number
  -> Map.Map Attr Number
  -> Role
  -> Map.Map Attr (Tuple Number Number)
zipRoleAttributes secondaryWeight attrs { primary, secondary } = Map.union
  (Map.intersectionWith Tuple attrs (foo 1.0 primary))
  (Map.intersectionWith Tuple attrs (foo secondaryWeight secondary))
  where
  foo w xs = Array.fromFoldable xs
    # map (\attr -> Tuple attr w)
    # Map.fromFoldable

allRoles :: Array Role
allRoles =
  [
    -- Keeper
    { name: "Goalkeeper (Defend)"
    , group: SG.GK
    , positions: Pos.unsafeParse "GK"
    , primary: Set.fromFoldable [ Aer, Cmd, Com, Han, Kic, Ref, Cnt, Pos, Agi ]
    , secondary: Set.fromFoldable [ OneV1, Thr, Ant, Dec ]
    }
  , { name: "Sweeper Keeper (Defend)"
    , group: SG.GK
    , positions: Pos.unsafeParse "GK"
    , primary: Set.fromFoldable [ Cmd, Kic, OneV1, Ref, Ant, Cnt, Pos, Agi ]
    , secondary: Set.fromFoldable [ Aer, Com, Fir, Ecc, Han, Pas, Tro, Thr, Cmp, Dec, Vis, Acc ]
    }
  , { name: "Sweeper Keeper (Support)"
    , group: SG.GK
    , positions: Pos.unsafeParse "GK"
    , primary: Set.fromFoldable [ Cmd, Kic, OneV1, Ref, Tro, Ant, Cmp, Cnt, Pos, Agi ]
    , secondary: Set.fromFoldable [ Aer, Com, Fir, Han, Pas, Thr, Dec, Vis, Acc ]
    }
  , { name: "Sweeper Keeper (Attack)"
    , group: SG.GK
    , positions: Pos.unsafeParse "GK"
    , primary: Set.fromFoldable [ Cmd, Kic, OneV1, Ref, Tro, Ant, Cmp, Cnt, Pos, Agi ]
    , secondary: Set.fromFoldable [ Aer, Com, Fir, Ecc, Han, Pas, Thr, Dec, Vis, Acc ]
    }
  -- Defender

  , { name: "Ball Playing Defender (Defend)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Hea, Mar, Pas, Tck, Cmp, Pos, Jum, Str ]
    , secondary: Set.fromFoldable [ Fir, Tec, Agg, Ant, Bra, Cnt, Dec, Vis, Pac ]
    }
  , { name: "Ball Playing Defender (Stopper)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Hea, Pas, Tck, Agg, Bra, Cmp, Dec, Pos, Jum, Str ]
    , secondary: Set.fromFoldable [ Fir, Mar, Tec, Ant, Cnt, Vis ]
    }
  , { name: "Ball Playing Defender (Cover)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Mar, Pas, Tck, Ant, Cmp, Cnt, Dec, Pos, Pac ]
    , secondary: Set.fromFoldable [ Fir, Hea, Tec, Bra, Vis, Jum, Str ]
    }

  , { name: "Central Defender (Defend)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Hea, Mar, Tck, Pos, Jum, Str ]
    , secondary: Set.fromFoldable [ Agg, Ant, Bra, Cmp, Cnt, Dec, Pac ]
    }
  , { name: "Central Defender (Stopper)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Hea, Tck, Agg, Bra, Dec, Pos, Jum, Str ]
    , secondary: Set.fromFoldable [ Mar, Ant, Cmp, Cnt ]
    }
  , { name: "Central Defender (Cover)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Mar, Tck, Ant, Cnt, Dec, Pos, Pac ]
    , secondary: Set.fromFoldable [ Hea, Bra, Cmp, Jum, Str ]
    }
  -- Complete Wingback (Support/Attack)
  , { name: "Fullback (Defend)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR)"
    , primary: Set.fromFoldable [ Mar, Tck, Ant, Cnt, Pos ]
    , secondary: Set.fromFoldable [ Cro, Pas, Dec, Tea, Wor, Pac, Sta ]
    }
  , { name: "Fullback (Support)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR)"
    , primary: Set.fromFoldable [ Mar, Tck, Ant, Cnt, Pos, Tea ]
    , secondary: Set.fromFoldable [ Cro, Dri, Pas, Tec, Dec, Wor, Pac, Sta ]
    }
  , { name: "Fullback (Attack)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR)"
    , primary: Set.fromFoldable [ Cro, Mar, Tck, Ant, Pos, Tea ]
    , secondary: Set.fromFoldable [ Dri, Fir, Pas, Tec, Cnt, Dec, OtB, Wor, Agi, Pac, Sta ]
    }
  -- Inverted Fullback (Defend)
  -- Inverted Wingback (Defend/Support/Attack)
  -- Libero (Defend/Support)
  -- No-nonsense Centerback (Defend/Stopper/Cover)
  -- No-nonsense Fullback (Defend)
  -- Wide Centerback (Defend/Support/Attack)
  , { name: "Wingback (Defend)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR), DM (LR)"
    , primary: Set.fromFoldable [ Mar, Tck, Ant, Pos, Tea, Wor, Acc, Sta ]
    , secondary: Set.fromFoldable [ Cro, Dri, Fir, Pas, Tec, Cnt, Dec, OtB, Agi, Bal, Pac ]
    }
  , { name: "Wingback (Support)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR), DM (LR)"
    , primary: Set.fromFoldable [ Cro, Dri, Mar, Tck, OtB, Tea, Wor, Acc, Sta ]
    , secondary: Set.fromFoldable [ Fir, Pas, Tec, Ant, Cnt, Dec, Pos, Agi, Bal, Pac ]
    }
  , { name: "Wingback (Attack)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR), DM (LR)"
    , primary: Set.fromFoldable [ Cro, Dri, Tck, Tec, OtB, Tea, Wor, Acc, Pac, Sta ]
    , secondary: Set.fromFoldable [ Fir, Mar, Pas, Ant, Cnt, Dec, Fla, Pos, Agi, Bal ]
    }

  -- Midfield

  -- Advanced Playmaker (Support/Attack)
  -- Anchor (Defend)
  -- Attacking Midfielder (Support/Attack)
  -- Ball Winning Midfielder (Defend/Support/Attack)
  -- Box to Box Midfielder (Support)
  -- Carrilero (Support)
  , { name: "Central Midfielder (Defend)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M"
    , primary: Set.fromFoldable [ Tck, Cnt, Dec, Pos, Tea ]
    , secondary: Set.fromFoldable [ Fir, Mar, Pas, Tec, Agg, Ant, Cmp, Wor, Sta ]
    }
  , { name: "Central Midfielder (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M"
    , primary: Set.fromFoldable [ Fir, Pas, Tck, Dec, Tea ]
    , secondary: Set.fromFoldable [ Tec, Ant, Cmp, Cnt, OtB, Vis, Wor, Sta ]
    }
  , { name: "Central Midfielder (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M"
    , primary: Set.fromFoldable [ Fir, Pas, Dec, OtB ]
    , secondary: Set.fromFoldable [ Lon, Tck, Tec, Ant, Cmp, Tea, Vis, Wor, Acc, Sta ]
    }
  -- Deeplying Playmaker (Defend/Support)
  -- Defensive Midfielder (Defend/Support)
  -- Defensive Winger (Defend/Support)
  -- Enganche (Support)
  -- Halfback (Defend)
  -- Inside Forward (Support/Attack)
  -- Inverted Winger (Support/Attack)
  -- Mezzala (Support/Attack)
  -- Raumdeuter (Attack)
  -- Regista (Support)
  -- Roaming Playmaker (Support)
  -- Segundo Volante (Support/Attack)
  -- Shadow Striker (Attack)
  -- Wide Midfielder (Defend/Support/Attack)
  -- Wide Playmaker (Support/Attack)
  -- Wide Target Forward (Support/Attack)
  -- Winger (Support/Attack)
  , { name: "Winger (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M (LR), AM (LR)"
    , primary: Set.fromFoldable [ Cro, Dri, Tec, Acc, Agi ]
    , secondary: Set.fromFoldable [ Fir, Pas, OtB, Wor, Bal, Pac, Str ]
    }
  , { name: "Winger (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M (LR), AM (LR)"
    , primary: Set.fromFoldable [ Cro, Dri, Tec, Acc, Agi ]
    , secondary: Set.fromFoldable [ Fir, Pas, Ant, Fla, OtB, Wor, Bal, Pac, Str ]
    }

  -- Attack

  , { name: "Advance Forward (Attack)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Dri, Fin, Fir, Tec, Cmp, OtB, Acc ]
    , secondary: Set.fromFoldable [ Pas, Ant, Dec, Wor, Agi, Bal, Pac, Sta ]
    }
  -- Complete Forward (Support/Attack)
  -- Deeplying Forward (Support/Attack)
  -- False Nine (Support)
  , { name: "Poacher (Attack)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Fin, Ant, Cmp, OtB ]
    , secondary: Set.fromFoldable [ Fir, Hea, Tec, Dec, Acc ]
    }
  -- Pressing Forwards (Defend/Support/Attack)
  , { name: "Target Forward (Support)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Hea, Bra, Tea, Bal, Jum, Str ]
    , secondary: Set.fromFoldable [ Fin, Fir, Agg, Ant, Cmp, Dec, OtB ]
    }
  , { name: "Target Forward (Attack)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Fin, Hea, Bra, Cmp, OtB, Bal, Jum, Str ]
    , secondary: Set.fromFoldable [ Fir, Agg, Ant, Dec, Tea ]
    }
  -- Trequartista (Attack)

  ]

