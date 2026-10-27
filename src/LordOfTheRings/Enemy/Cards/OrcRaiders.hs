module LordOfTheRings.Enemy.Cards.OrcRaiders (orcRaiders) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrcRaiders = OrcRaiders EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcRaiders :: EnemyCard OrcRaiders
orcRaiders = enemy OrcRaiders Cards.orcRaiders

-- | Ambush.
-- Forced: After Orc Raiders engages a player, that player discards 2 attachments he
-- controls, if able.
instance HasAbilities OrcRaiders where
  getAbilities (OrcRaiders a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage OrcRaiders where
  runMessage msg e@(OrcRaiders attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        attachments <- getAttachmentsControlledBy pid
        if length attachments <= 2
          then for_ attachments discard
          else chooseOneM pid do
            questionLabeled "chooseCardToDiscard"
            causedBy attrs
            targets attachments \chosen -> do
              discard chosen
              chooseAndDiscard attrs pid (filter (/= chosen) attachments)
      pure e
    _ -> OrcRaiders <$> runMessage msg attrs
