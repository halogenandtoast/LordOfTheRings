module LordOfTheRings.Enemy.Cards.RhunBackstabber (rhunBackstabber) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype RhunBackstabber = RhunBackstabber EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rhunBackstabber :: EnemyCard RhunBackstabber
rhunBackstabber = enemy RhunBackstabber Cards.rhunBackstabber

-- | Forced: After Rhûn Backstabber engages you, either raise your threat by 2, or discard an
-- ally you control with printed cost 2 or higher.
instance HasAbilities RhunBackstabber where
  getAbilities (RhunBackstabber a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Raise your threat by 1 for each enemy engaged with you.
instance RunMessage RhunBackstabber where
  runMessage msg e@(RhunBackstabber attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        allies <- filterM (fmap (cardMatches (CardWithPrintedCostAtLeast 2)) . getAlly) =<< select (AllyControlledBy $ PlayerWithId pid)
        chooseOneM pid do
          questionLabeled "rhunBackstabber.choose"
          causedBy attrs
          labeled "rhunBackstabber.raiseThreat" $ encounterRaisesThreat pid 2
          unless (null allies) $ labeled "rhunBackstabber.discardAlly" do
            chooseOrRunOneM pid do
              questionLabeled "rhunBackstabber.ally"
              causedBy attrs
              targets allies (discard attrs)
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      n <- length <$> select (EnemyEngagedWith $ PlayerWithId pid)
      encounterRaisesThreat pid n
      pure e
    _ -> RhunBackstabber <$> runMessage msg attrs
