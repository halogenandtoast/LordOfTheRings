module LordOfTheRings.Enemy.Cards.SpawnOfShelob (spawnOfShelob) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SpawnOfShelob = SpawnOfShelob EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge, as its card def says.
spawnOfShelob :: EnemyCard SpawnOfShelob
spawnOfShelob = enemy SpawnOfShelob Cards.spawnOfShelob

-- | Forced: After Spawn of Shelob engages you, either discard the top card of your deck,
-- or Spawn of Shelob gets +2 Attack until the end of the round. The engaged player
-- chooses; the discard is offered only with a card to discard (FAQ 1.44).
instance HasAbilities SpawnOfShelob where
  getAbilities (SpawnOfShelob a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Exhaust a character you control.
instance RunMessage SpawnOfShelob where
  runMessage msg e@(SpawnOfShelob attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyEngaged _ pid) <- ws]) \pid -> do
        deck <- (.deck) <$> getPlayer pid
        chooseOrRunOneM pid do
          questionLabeled "spawnOfShelob.choose"
          causedBy attrs
          unless (null deck) $ labeled "spawnOfShelob.discard" $ discardTopOfDeck pid 1
          labeled "spawnOfShelob.attack" $ roundModifier attrs attrs (AttackModifier 2)
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure e
    _ -> SpawnOfShelob <$> runMessage msg attrs
