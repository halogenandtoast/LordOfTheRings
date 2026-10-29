module LordOfTheRings.Ally.Cards.Bofur (bofur) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype Bofur = Bofur AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Quest Action: Spend 1 spirit resource to put Bofur into play from your hand,
-- exhausted and committed to a quest.
bofur :: AllyCard Bofur
bofur =
  ally
    (\a -> Bofur a {allyHandAction = Just $ DuringPhase QuestPhase <> ResourcesOfSphere (PlayerWithId a.owner) Spirit 1})
    Cards.bofur

-- | If you quest successfully this phase and Bofur is still in play, return him to your
-- hand.
instance HasAbilities Bofur where
  getAbilities (Bofur a) =
    [ restricted a 1 (TargetHasModifier (CardIdTarget a.allyCardId) ReturnToHandIfQuestedSuccessfully)
        $ silent
        $ QuestedSuccessfullyWindow After
    ]

instance RunMessage Bofur where
  runMessage msg s@(Bofur a) = case msg of
    UseHandAction pid cid | cid == a.allyCardId -> do
      spendResources pid Spirit 1
      putIntoPlayCommitted pid cid
      phaseModifier a (CardIdTarget cid) ReturnToHandIfQuestedSuccessfully
      doStep 1 msg
      pure s
    -- put into play once the players quested successfully, he goes straight back (rulings
    -- 3.131 and 3.211); the Bofur in play reads this
    DoStep 1 (UseHandAction _ cid) | cid == a.allyCardId -> do
      whenM (elem QuestedSuccessfullyThisPhase <$> getModifiers GameTarget) $ returnToHand a
      pure s
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      returnToHand a
      pure s
    _ -> Bofur <$> runMessage msg a
