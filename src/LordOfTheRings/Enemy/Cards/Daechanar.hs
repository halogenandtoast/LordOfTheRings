module LordOfTheRings.Enemy.Cards.Daechanar (daechanar) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Scenario.Scenarios.TheDreadRealm.Helpers (sorceryCardsInPlay)

newtype Daechanar = Daechanar EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

daechanar :: EnemyCard Daechanar
daechanar = enemy Daechanar Cards.daechanar

-- | Cannot have attachments. Daechanar gets +1 attack for each Sorcery card in play, and
-- gains indestructible while at least 1 Sorcery card is in play.
instance HasModifiersFor Daechanar where
  getModifiersFor (Daechanar a) = do
    n <- length <$> sorceryCardsInPlay
    modifySelf a $ CannotHaveAttachments : [m | n > 0, m <- [AttackModifier n, GainKeyword Indestructible]]

-- | Forced: When Daechanar is damaged, if there is at least 1 Sorcery card in play, cancel
-- all damage just dealt and remove a Sorcery card from the game.
instance HasAbilities Daechanar where
  getAbilities (Daechanar a) = [mkAbility a 1 $ forced $ DealtDamageWindow When (toTarget a)]

-- | The first player chooses which Sorcery card leaves.
instance RunMessage Daechanar where
  runMessage msg e@(Daechanar attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      sorcery <- sorceryCardsInPlay
      unless (null sorcery) do
        cancelDamage attrs
        lead <- getLead
        chooseOrRunOneM lead do
          questionLabeled "daechanar.removeSorcery"
          causedBy attrs
          targets sorcery removeFromGame
      pure e
    _ -> Daechanar <$> runMessage msg attrs
