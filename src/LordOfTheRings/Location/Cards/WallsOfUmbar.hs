module LordOfTheRings.Location.Cards.WallsOfUmbar (wallsOfUmbar) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

-- | Its *Forced, as a Sailing test discards it, is The City of Corsairs'.
newtype WallsOfUmbar = WallsOfUmbar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wallsOfUmbar :: LocationCard WallsOfUmbar
wallsOfUmbar = location WallsOfUmbar Cards.wallsOfUmbar

-- | While Walls of Umbar is in the staging area, it gains: "Forced: After a player commits
-- an ally to a Sailing test, raise his or her threat by 1."
instance HasAbilities WallsOfUmbar where
  getAbilities (WallsOfUmbar a) = [mkAbility a 1 $ forced $ CommittedToSailingTestWindow After | a.placement == StagingArea]

instance RunMessage WallsOfUmbar where
  runMessage msg l@(WallsOfUmbar a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [(pid, n) | Window _ (CommittedToSailingTest pid cs) <- ws, let n = length [() | AllyTarget _ <- cs], n > 0] \(pid, n) ->
        raiseThreat pid n
      pure l
    _ -> WallsOfUmbar <$> runMessage msg a
