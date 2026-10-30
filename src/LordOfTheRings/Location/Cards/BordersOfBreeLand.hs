module LordOfTheRings.Location.Cards.BordersOfBreeLand (bordersOfBreeLand) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype BordersOfBreeLand = BordersOfBreeLand LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bordersOfBreeLand :: LocationCard BordersOfBreeLand
bordersOfBreeLand = location BordersOfBreeLand Cards.bordersOfBreeLand

-- | While Borders of Bree-land is in the staging area, it gains: "Forced: At the beginning
-- of the quest phase, return 1 enemy engaged with a player to the staging area."
instance HasAbilities BordersOfBreeLand where
  getAbilities (BordersOfBreeLand a) =
    [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | a.placement == StagingArea]

-- | The first player chooses the enemy.
-- Shadow: Defending character gets -1 defense for each quest stage in play: each quest card,
-- the side quests too.
instance RunMessage BordersOfBreeLand where
  runMessage msg l@(BordersOfBreeLand a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      engaged <- select (EnemyEngagedWith AnyPlayer)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "bordersOfBreeLand.return"
        causedBy a
        targets engaged \e -> placeEnemy e StagingArea
      pure l
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      n <- questCardsInPlay
      for_ defenders \d -> attackModifier (CardIdSource cid) d (DefenseModifier (negate n))
      pure l
    _ -> BordersOfBreeLand <$> runMessage msg a
