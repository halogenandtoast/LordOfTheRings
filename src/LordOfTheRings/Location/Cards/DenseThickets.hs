module LordOfTheRings.Location.Cards.DenseThickets (denseThickets) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (InVictoryDisplay, StagingArea, Unplaced))
import LordOfTheRings.Trait (Trait (Forest, Ranger))

newtype DenseThickets = DenseThickets LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

denseThickets :: LocationCard DenseThickets
denseThickets = location DenseThickets Cards.denseThickets

-- | While Dense Thickets is in the staging area, each Forest location in play gets +1
-- threat.
instance HasModifiersFor DenseThickets where
  getModifiersFor (DenseThickets a) =
    when (a.placement == StagingArea) do
      forests <- select (LocationWithTrait Forest)
      for_ forests \l -> modified a l [ThreatModifier 1]

-- | Shadow: Excess damage dealt by this attack (damage that is dealt beyond the remaining
-- hit points of the character damaged by this attack) must be assigned to a Ranger
-- objective in play.
instance RunMessage DenseThickets where
  runMessage msg l@(DenseThickets a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      rangers <-
        getsGame \g ->
          [ toTarget o
          | o <- toList g.entities.objectives
          , Ranger `member` cdCardTraits (toCardDef o)
          , o.placement `notElem` [InVictoryDisplay, Unplaced]
          ]
      for_ rangers \r -> attackModifier a eid (ExcessDamageTo r)
      pure l
    _ -> DenseThickets <$> runMessage msg a
