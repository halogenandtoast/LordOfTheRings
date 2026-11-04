module LordOfTheRings.Enemy.Cards.JailorGornakhNightmare (jailorGornakhNightmare) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (captureHeroesFromDecks)

newtype JailorGornakhNightmare = JailorGornakhNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

jailorGornakhNightmare :: EnemyCard JailorGornakhNightmare
jailorGornakhNightmare = enemy JailorGornakhNightmare Cards.jailorGornakhNightmare

-- | Forced: After Jailor Gornákh enters play, each player searches his captured deck for
-- each hero and captures them underneath Jailor Gornákh.
-- Forced: After a player rescues any number of cards, Jailor Gornákh attacks that player.
instance HasAbilities JailorGornakhNightmare where
  getAbilities (JailorGornakhNightmare a) =
    [ mkAbility a 1 $ forced $ CardEntersStagingAreaWindow After (toTarget a)
    , mkAbility a 1 $ forced $ EntersPlayWindow After (toTarget a)
    , mkAbility a 2 $ forced $ CapturedCardsRescuedWindow After
    ]

-- | Captured cards are rescued by their owners (Escape from Mount Gram rules), so the
-- players attacked are the owners of the cards rescued.
instance RunMessage JailorGornakhNightmare where
  runMessage msg e@(JailorGornakhNightmare attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      captureHeroesFromDecks attrs
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 ws -> do
      let rescuers = ordNub [owner | Window _ (CapturedCardsRescued _ cards) <- ws, PlayerCard pc <- cards, Just owner <- [pc.owner]]
      for_ rescuers \pid -> immediateAttack pid attrs.id
      pure e
    _ -> JailorGornakhNightmare <$> runMessage msg attrs
