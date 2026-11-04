module LordOfTheRings.Location.Cards.DeepRavine (deepRavine) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DeepRavine = DeepRavine LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

deepRavine :: LocationCard DeepRavine
deepRavine = location DeepRavine Cards.deepRavine

{- | While Deep Ravine is in the staging area, no more than 1 progress can be placed on
locations in the staging area each round: each one there may take, this round, what the
others there have not taken already.
-}
instance HasModifiersFor DeepRavine where
  getModifiersFor (DeepRavine a) = when (a.placement == StagingArea) do
    staged <- select LocationInStagingArea
    -- the count each keeps, read from the effects: the modifiers are being built
    effects <- getsGame gameEffects
    let placedOn l = sum [n | e <- effects, effectTarget e == toTarget l, ProgressPlacedThisRound n <- effectModifiers e]
        placed = [(l, placedOn l) | l <- staged]
        total = sum (map snd placed)
    for_ placed \(l, own) -> modified a l [ProgressPerRoundLimit (max 0 (1 - (total - own)))]

-- | Travel: The first player must either exhaust Urdug, or raise each player's threat by 2
-- to travel here.
instance RunMessage DeepRavine where
  runMessage msg l@(DeepRavine a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      urdug <- select (AllyWithCardCode (toCardCode Objectives.urdug) <> AllyControlledBy (PlayerWithId lead) <> ReadyAlly)
      chooseOrRunOneM lead do
        questionLabeled "deepRavine.travel"
        causedBy a
        for_ urdug \u -> labeled "deepRavine.exhaustUrdug" $ exhaust u
        labeled "deepRavine.raiseThreat" $ eachPlayer \p -> raiseThreat p 2
      pure l
    _ -> DeepRavine <$> runMessage msg a
