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

  , { name: "Complete Wingback (Support)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR), DM (LR)"
    , primary: Set.fromFoldable [ Cro, Dri, Tec, OtB, Tea, Wor, Acc, Sta ]
    , secondary: Set.fromFoldable [ Fir, Mar, Pas, Tck, Ant, Dec, Fla, Pos, Agi, Bal, Pac ]
    }
  , { name: "Complete Wingback (Attack)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR), DM (LR)"
    , primary: Set.fromFoldable [ Cro, Dri, Tec, Fla, OtB, Tea, Wor, Acc, Sta ]
    , secondary: Set.fromFoldable [ Fir, Mar, Pas, Tck, Ant, Dec, Pos, Agi, Bal, Pac ]
    }

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

  , { name: "Inverted Fullback (Defend)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR)"
    , primary: Set.fromFoldable [ Hea, Mar, Tck, Pos, Str ]
    , secondary: Set.fromFoldable [ Dri, Fir, Pas, Tec, Agg, Ant, Bra, Cmp, Cnt, Dec, Wor, Agi, Jum, Pac ]
    }

  , { name: "Inverted Wingback (Defend)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR), DM (LR)"
    , primary: Set.fromFoldable [ Pas, Tck, Ant, Dec, Pos, Tea ]
    , secondary: Set.fromFoldable [ Fir, Mar, Tec, Cmp, Cnt, OtB, Wor, Acc, Agi ]
    }
  , { name: "Inverted Wingback (Support)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR), DM (LR)"
    , primary: Set.fromFoldable [ Fir, Pas, Tck, Cmp, Dec, Tea ]
    , secondary: Set.fromFoldable [ Mar, Tec, Ant, Cnt, OtB, Pos, Vis, Wor, Acc, Agi, Sta ]
    }
  , { name: "Inverted Wingback (Attack)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR), DM (LR)"
    , primary: Set.fromFoldable [ Fir, Pas, Tck, Tec, Cmp, Dec, OtB, Tea, Vis, Acc ]
    , secondary: Set.fromFoldable [ Cro, Dri, Lon, Mar, Ant, Cnt, Fla, Pos, Wor, Agi, Pac, Sta ]
    }

  , { name: "Libero (Defend)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Fir, Hea, Mar, Pas, Tck, Tec, Cmp, Dec, Pos, Tea, Jum, Str ]
    , secondary: Set.fromFoldable [ Ant, Bra, Cnt, Pac, Sta ]
    }
  , { name: "Libero (Support)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Fir, Hea, Mar, Pas, Tck, Tec, Cmp, Dec, Pos, Tea, Jum, Str ]
    , secondary: Set.fromFoldable [ Dri, Ant, Bra, Cnt, Vis, Pac, Sta ]
    }

  , { name: "No-nonsence Centerback (Defend)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Hea, Mar, Tck, Pos, Jum, Str ]
    , secondary: Set.fromFoldable [ Agg, Ant, Bra, Cnt, Pac ]
    }
  , { name: "No-nonsence Centerback (Stopper)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Hea, Tck, Agg, Bra, Pos, Jum, Str ]
    , secondary: Set.fromFoldable [ Mar, Ant, Cnt ]
    }
  , { name: "No-nonsence Centerback (Cover)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Mar, Tck, Ant, Cnt, Pos, Pac ]
    , secondary: Set.fromFoldable [ Hea, Bra, Jum, Str ]
    }

  , { name: "No-nonsense Fullback (Defend)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D (LR)"
    , primary: Set.fromFoldable [ Mar, Tck, Ant, Pos, Str ]
    , secondary: Set.fromFoldable [ Hea, Agg, Bra, Cnt, Tea ]
    }

  , { name: "Wide Centerback (Defend)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Hea, Mar, Tck, Pos, Jum, Str ]
    , secondary: Set.fromFoldable [ Dri, Fir, Pas, Tec, Agg, Ant, Bra, Cmp, Cnt, Dec, Wor, Agi, Pac ]
    }
  , { name: "Wide Centerback (Support)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Dri, Hea, Mar, Tck, Pos, Jum, Pac, Str ]
    , secondary: Set.fromFoldable [ Cro, Fir, Pas, Tec, Agg, Ant, Bra, Cmp, Cnt, Dec, OtB, Wor, Agi, Sta ]
    }
  , { name: "Wide Centerback (Attack)"
    , group: SG.Def
    , positions: Pos.unsafeParse "D"
    , primary: Set.fromFoldable [ Cro, Dri, Hea, Mar, Tck, OtB, Jum, Pac, Sta, Str ]
    , secondary: Set.fromFoldable [ Fir, Pas, Tec, Agg, Ant, Bra, Cmp, Cnt, Dec, Pos, Wor, Agi ]
    }

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
  , { name: "Advanced Playmaker (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M, AM"
    , primary: Set.fromFoldable [ Fir, Pas, Tec, Cmp, Dec, OtB, Tea, Vis ]
    , secondary: Set.fromFoldable [ Dri, Ant, Fla, Agi ]
    }
  , { name: "Advanced Playmaker (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M, AM"
    , primary: Set.fromFoldable [ Fir, Pas, Tec, Cmp, Dec, OtB, Tea, Vis ]
    , secondary: Set.fromFoldable [ Dri, Ant, Fla, Acc, Agi ]
    }

  , { name: "Anchor (Defend)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "DM"
    , primary: Set.fromFoldable [ Mar, Tck, Ant, Cnt, Dec, Pos ]
    , secondary: Set.fromFoldable [ Cmp, Tea, Str ]
    }

  , { name: "Advanced Playmaker (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "AM"
    , primary: Set.fromFoldable [ Fir, Lon, Pas, Tec, Ant, Dec, Fla, OtB ]
    , secondary: Set.fromFoldable [ Dri, Cmp, Vis, Agi ]
    }
  , { name: "Advanced Playmaker (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "AM"
    , primary: Set.fromFoldable [ Dri, Fir, Lon, Pas, Tec, Ant, Dec, Fla, OtB ]
    , secondary: Set.fromFoldable [ Fin, Cmp, Vis, Agi ]
    }

  , { name: "Ball Winning Midfielder (Defend)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "DM, M"
    , primary: Set.fromFoldable [ Tck, Agg, Ant, Tea, Wor, Sta ]
    , secondary: Set.fromFoldable [ Mar, Bra, Cnt, Pos, Agi, Pac, Str ]
    }
  , { name: "Ball Winning Midfielder (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "DM, M"
    , primary: Set.fromFoldable [ Tck, Agg, Ant, Tea, Wor, Sta ]
    , secondary: Set.fromFoldable [ Mar, Pas, Bra, Cnt, Agi, Pac, Str ]
    }

  , { name: "Ball Winning Midfielder (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M"
    , primary: Set.fromFoldable [ Pas, Tck, OtB, Tea, Wor, Sta ]
    , secondary: Set.fromFoldable [ Dri, Fin, Fir, Lon, Tec, Agg, Ant, Cmp, Dec, Pos, Acc, Bal, Pac, Str ]
    }

  , { name: "Carrilero (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M"
    , primary: Set.fromFoldable [ Fir, Pas, Tck, Dec, Pos, Tea, Sta ]
    , secondary: Set.fromFoldable [ Tec, Ant, Cmp, Cnt, OtB, Vis, Wor ]
    }

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

  , { name: "Deeplying Playmaker (Defend)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "DM, M"
    , primary: Set.fromFoldable [ Fir, Pas, Tec, Cmp, Dec, Tea, Vis ]
    , secondary: Set.fromFoldable [ Tck, Ant, Pos, Bal ]
    }
  , { name: "Deeplying Playmaker (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "DM, M"
    , primary: Set.fromFoldable [ Fir, Pas, Tec, Cmp, Dec, Tea, Vis ]
    , secondary: Set.fromFoldable [ Ant, OtB, Pos, Bal ]
    }

  , { name: "Defensive Midfielder (Defend)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "DM"
    , primary: Set.fromFoldable [ Tck, Ant, Cnt, Pos, Tea ]
    , secondary: Set.fromFoldable [ Mar, Pas, Agg, Cmp, Dec, Wor, Sta, Str ]
    }
  , { name: "Defensive Midfielder (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "DM"
    , primary: Set.fromFoldable [ Tck, Ant, Cnt, Pos, Tea ]
    , secondary: Set.fromFoldable [ Fir, Mar, Pas, Agg, Cmp, Dec, Wor, Sta, Str ]
    }

  , { name: "Defensive Winger (Defend)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M (LR)"
    , primary: Set.fromFoldable [ Tec, Ant, OtB, Pos, Tea, Wor, Sta ]
    , secondary: Set.fromFoldable [ Cro, Dri, Fir, Mar, Tck, Agg, Cnt, Dec, Acc ]
    }
  , { name: "Defensive Winger (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M (LR)"
    , primary: Set.fromFoldable [ Cro, Tec, OtB, Tea, Wor, Sta ]
    , secondary: Set.fromFoldable [ Dri, Fir, Mar, Pas, Tck, Agg, Ant, Cmp, Cnt, Dec, Pos, Acc ]
    }

  , { name: "Enganche (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "AM"
    , primary: Set.fromFoldable [ Fir, Pas, Tec, Cmp, Dec, Vis ]
    , secondary: Set.fromFoldable [ Dri, Ant, Fla, OtB, Tea, Agi ]
    }

  , { name: "Halfback (Defend)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "DM"
    , primary: Set.fromFoldable [ Mar, Tck, Ant, Cmp, Cnt, Dec, Pos, Tea ]
    , secondary: Set.fromFoldable [ Fir, Pas, Agg, Bra, Wor, Jum, Sta, Str ]
    }

  , { name: "Inside Forward (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "AM (LR)"
    , primary: Set.fromFoldable [ Dri, Fin, Fir, Tec, Acc, Agi ]
    , secondary: Set.fromFoldable [ Lon, Pas, Ant, Cmp, Fla, Vis, Wor, Bal, Pac, Sta ]
    }
  , { name: "Inside Forward (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "AM (LR)"
    , primary: Set.fromFoldable [ Dri, Fin, Fir, Tec, Ant, OtB, Acc, Agi ]
    , secondary: Set.fromFoldable [ Lon, Pas, Cmp, Fla, Wor, Bal, Pac, Sta ]
    }

  , { name: "Inverted Winger (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M (LR), AM (LR)"
    , primary: Set.fromFoldable [ Cro, Dri, Pas, Tec, Acc, Agi ]
    , secondary: Set.fromFoldable [ Fir, Lon, Cmp, Dec, OtB, Vis, Wor, Bal, Pac, Sta ]
    }
  , { name: "Inverted Winger (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M (LR), AM (LR)"
    , primary: Set.fromFoldable [ Cro, Dri, Pas, Tec, Acc, Agi ]
    , secondary: Set.fromFoldable [ Fir, Lon, Ant, Cmp, Dec, Fla, OtB, Vis, Wor, Bal, Pac, Sta ]
    }

  , { name: "Mezzala (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M"
    , primary: Set.fromFoldable [ Pas, Tec, Dec, OtB, Wor, Acc ]
    , secondary: Set.fromFoldable [ Dri, Fir, Lon, Tck, Ant, Cmp, Vis, Bal, Sta ]
    }
  , { name: "Mezzala (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M"
    , primary: Set.fromFoldable [ Dri, Pas, Tec, Dec, OtB, Vis, Wor, Acc ]
    , secondary: Set.fromFoldable [ Fin, Fir, Lon, Ant, Fla, Bal, Sta ]
    }

  , { name: "Raumdeuter (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "AM (LR)"
    , primary: Set.fromFoldable [ Fin, Ant, Cmp, Cnt, Dec, OtB, Bal ]
    , secondary: Set.fromFoldable [ Fir, Tec, Wor, Acc, Sta ]
    }

  , { name: "Regista (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "DM"
    , primary: Set.fromFoldable [ Fir, Pas, Tec, Cmp, Dec, Fla, OtB, Tea, Vis ]
    , secondary: Set.fromFoldable [ Dri, Lon, Ant, Bal ]
    }

  , { name: "Roaming Playmaker (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "DM, M"
    , primary: Set.fromFoldable [ Fir, Pas, Tec, Ant, Cmp, Dec, OtB, Tea, Vis, Wor, Acc, Sta ]
    , secondary: Set.fromFoldable [ Dri, Lon, Cnt, Pos, Agi, Bal, Pac ]
    }

  , { name: "Segundo Volante (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "DM"
    , primary: Set.fromFoldable [ Mar, Pas, Tck, OtB, Pos, Wor, Pac, Sta ]
    , secondary: Set.fromFoldable [ Fin, Fir, Lon, Ant, Cmp, Cnt, Dec, Acc, Bal, Str ]
    }
  , { name: "Segundo Volante (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "DM"
    , primary: Set.fromFoldable [ Fin, Lon, Pas, Tck, Ant, OtB, Pos, Wor, Pac, Sta ]
    , secondary: Set.fromFoldable [ Fir, Mar, Cmp, Cnt, Dec, Acc, Bal, Str ]
    }

  , { name: "Shadow Striker (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "AM"
    , primary: Set.fromFoldable [ Dri, Fin, Fir, Ant, Cmp, OtB, Acc ]
    , secondary: Set.fromFoldable [ Pas, Tec, Cnt, Dec, Wor, Agi, Bal, Pac, Sta ]
    }

  , { name: "Wide Midfielder (Defend)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M (LR)"
    , primary: Set.fromFoldable [ Pas, Tck, Cnt, Dec, Pos, Tea, Wor ]
    , secondary: Set.fromFoldable [ Cro, Fir, Mar, Tec, Ant, Cmp, Sta ]
    }
  , { name: "Wide Midfielder (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M (LR)"
    , primary: Set.fromFoldable [ Pas, Tck, Dec, Tea, Wor, Sta ]
    , secondary: Set.fromFoldable [ Cro, Fir, Tec, Ant, Cmp, Cnt, OtB, Pos, Vis ]
    }
  , { name: "Wide Midfielder (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M (LR)"
    , primary: Set.fromFoldable [ Cro, Fir, Pas, Dec, Tea, Wor, Sta ]
    , secondary: Set.fromFoldable [ Tck, Tec, Ant, Cmp, OtB, Vis ]
    }

  , { name: "Wide Playmaker (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M (LR)"
    , primary: Set.fromFoldable [ Fir, Pas, Tec, Cmp, Dec, Tea, Vis ]
    , secondary: Set.fromFoldable [ Dri, OtB, Agi ]
    }
  , { name: "Wide Playmaker (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "M (LR)"
    , primary: Set.fromFoldable [ Dri, Fir, Pas, Tec, Cmp, Dec, OtB, Tea, Vis ]
    , secondary: Set.fromFoldable [ Ant, Fla, Acc, Agi ]
    }

  , { name: "Wide Target Forward (Support)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "AM (LR)"
    , primary: Set.fromFoldable [ Hea, Bra, Tea, Jum, Str ]
    , secondary: Set.fromFoldable [ Cro, Fir, Ant, OtB, Wor, Bal, Sta ]
    }
  , { name: "Wide Target Forward (Attack)"
    , group: SG.Mid
    , positions: Pos.unsafeParse "AM (LR)"
    , primary: Set.fromFoldable [ Hea, Bra, OtB, Jum, Str ]
    , secondary: Set.fromFoldable [ Cro, Fin, Fir, Ant, Tea, Wor, Bal, Sta ]
    }

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

  , { name: "Complete Forward (Support)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Dri, Fir, Hea, Lon, Pas, Tec, Ant, Cmp, Dec, OtB, Vis, Acc, Agi, Str ]
    , secondary: Set.fromFoldable [ Fin, Tea, Wor, Bal, Jum, Pac, Sta ]
    }
  , { name: "Complete Forward (Attack)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Dri, Fin, Fir, Hea, Tec, Ant, Cmp, OtB, Acc, Agi, Str ]
    , secondary: Set.fromFoldable [ Lon, Pas, Dec, Tea, Vis, Wor, Bal, Jum, Pac, Sta ]
    }

  , { name: "Deeplying Forward (Support)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Fir, Pas, Tec, Cmp, Dec, OtB, Tea ]
    , secondary: Set.fromFoldable [ Fin, Ant, Fla, Vis, Bal, Str ]
    }
  , { name: "Deeplying Forward (Attack)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Fir, Pas, Tec, Cmp, Dec, OtB, Tea ]
    , secondary: Set.fromFoldable [ Dri, Fin, Ant, Fla, Vis, Bal, Str ]
    }

  , { name: "False Nine (Support)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Dri, Fir, Pas, Tec, Cmp, Dec, OtB, Vis, Acc, Agi ]
    , secondary: Set.fromFoldable [ Fin, Ant, Fla, Tea, Bal ]
    }

  , { name: "Poacher (Attack)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Fin, Ant, Cmp, OtB ]
    , secondary: Set.fromFoldable [ Fir, Hea, Tec, Dec, Acc ]
    }

  , { name: "Pressing Forward (Defend)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Agg, Ant, Bra, Dec, Tea, Wor, Acc, Pac, Sta ]
    , secondary: Set.fromFoldable [ Fir, Cmp, Cnt, Agi, Bal, Str ]
    }
  , { name: "Pressing Forward (Support)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Agg, Ant, Bra, Dec, Tea, Wor, Acc, Pac, Sta ]
    , secondary: Set.fromFoldable [ Fir, Pas, Cmp, Cnt, OtB, Agi, Bal, Str ]
    }
  , { name: "Pressing Forward (Attack)"
    , group: SG.Att
    , positions: Pos.unsafeParse "ST"
    , primary: Set.fromFoldable [ Agg, Ant, Bra, OtB, Tea, Wor, Acc, Pac, Sta ]
    , secondary: Set.fromFoldable [ Fin, Fir, Cmp, Cnt, Dec, Agi, Bal, Str ]
    }

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

  , { name: "Trequartista (Attack)"
    , group: SG.Att
    , positions: Pos.unsafeParse "AM, ST"
    , primary: Set.fromFoldable [ Dri, Fir, Pas, Tec, Cmp, Dec, Fla, OtB, Vis, Acc ]
    , secondary: Set.fromFoldable [ Fin, Ant, Agi, Bal ]
    }
  ]

