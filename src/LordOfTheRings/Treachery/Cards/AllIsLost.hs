module LordOfTheRings.Treachery.Cards.AllIsLost (allIsLost) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Card.Synthetic qualified as Synthetic
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AllIsLost = AllIsLost TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

allIsLost :: TreacheryCard AllIsLost
allIsLost = treachery AllIsLost Cards.allIsLost

reanimatedDead :: EnemyMatcher
reanimatedDead = EnemyWithCardCode (toCardCode Synthetic.reanimatedDead)

-- | When Revealed: Attach All is Lost to the first player's threat dial. (Counts as a
-- Condition attachment with the text: "Each time you attack a Reanimated Dead, raise your
-- threat by 1.") On the threat dial it lies in his play area, not on him, which would make
-- it a condition on his deck.
-- Shadow: If attacking enemy is a Reanimated Dead, it attacks again after this attack.
instance RunMessage AllIsLost where
  runMessage msg t@(AllIsLost attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      pure $ AllIsLost $ attrs {treacheryPlacement = InPlayArea lead}
    -- each attack, however many characters declared for it
    CheckWindows ws | InPlayArea pid <- attrs.placement -> do
      hits <- filterM (\(c, eid) -> (&&) <$> ((== Just pid) <$> characterController c) <*> selectAny (EnemyWithId eid <> reanimatedDead))
        $ ordNubBy snd (==) [(c, eid) | Window After (DeclaredAsAttacker c eid) <- ws]
      for_ hits \_ -> encounterRaisesThreat pid 1
      AllIsLost <$> runMessage msg attrs
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      whenM (selectAny $ EnemyWithId eid <> reanimatedDead) $ additionalAttack pid eid
      pure t
    _ -> AllIsLost <$> runMessage msg attrs
