module LordOfTheRings.Treachery.Cards.Overgrown (overgrown) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Overgrown = Overgrown TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge, as its card def says.
overgrown :: TreacheryCard Overgrown
overgrown = treachery Overgrown Cards.overgrown

-- | Counts as a Condition attachment with the text: "Attached location gets +1 threat and
-- gains 'Immune to player card effects.'"
instance HasModifiersFor Overgrown where
  getModifiersFor (Overgrown a) = withHost a \host -> modified a host [ThreatModifier 1, ImmuneToPlayerCardEffects]

-- | When Revealed: Attach to the highest threat location in the staging area without a
-- copy of Overgrown attached, the first player choosing among those tied. With none, it is
-- discarded.
instance RunMessage Overgrown where
  runMessage msg t@(Overgrown attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      overgrownHosts <- hostsOfTreachery (toCardCode attrs)
      locations <- filter ((`notElem` overgrownHosts) . toTarget) <$> select LocationInStagingArea
      scored <- traverse (\l -> (l,) <$> getLocationThreat l) locations
      let highest = foldr (max . snd) 0 scored
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "overgrown.attach"
        causedBy attrs
        targets [l | (l, n) <- scored, n == highest] $ attachTreachery attrs.treacheryId
      pure t
    _ -> Overgrown <$> runMessage msg attrs
