module LordOfTheRings.Quest.Cards.TheEnemysServant (theEnemysServant) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (
  bellachShielded,
  getBellach,
  getMould,
  mouldController,
  mouldOnHero,
  triggerEachScour,
 )

newtype TheEnemysServant = TheEnemysServant QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

theEnemysServant :: QuestCard TheEnemysServant
theEnemysServant = quest TheEnemysServant Cards.theEnemysServant

-- | Unless there is at least 12 progress on this stage, Bellach is immune to player card
-- effects and cannot be engaged.
instance HasModifiersFor TheEnemysServant where
  getModifiersFor (TheEnemysServant a) = when (a.progress < 12) $ bellachShielded a

-- | Time 3. Forced: After the last time counter is removed from this stage, trigger each
-- Scour effect currently in play. Place 3 time counters on this stage.
instance HasAbilities TheEnemysServant where
  getAbilities (TheEnemysServant a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | When Revealed: Trigger each Scour effect currently in play. Bellach makes an attack
-- against the player who controls Celebrimbor's Mould. Then, attach Celebrimbor's Mould to
-- Bellach.
-- The players cannot win unless Celebrimbor's Mould is attached to a hero. If the players
-- defeat this stage, they win the game.
instance RunMessage TheEnemysServant where
  runMessage msg q@(TheEnemysServant attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheEnemysServant <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      triggerEachScour
      bellach <- getBellach
      controller <- mouldController
      for_ ((,) <$> controller <*> bellach) (uncurry immediateAttack)
      mould <- getMould
      for_ ((,) <$> mould <*> bellach) \(o, b) -> attachObjective (toId o) b
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      triggerEachScour
      placeTimeCounters attrs attrs 3
      pure q
    _ -> TheEnemysServant <$> runConditionalStage mouldOnHero msg attrs
