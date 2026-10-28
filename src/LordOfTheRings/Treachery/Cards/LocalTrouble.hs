module LordOfTheRings.Treachery.Cards.LocalTrouble (localTrouble) where

import LordOfTheRings.Card (cdThreatCost, toCardDef)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LocalTrouble = LocalTrouble TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

localTrouble :: TreacheryCard LocalTrouble
localTrouble = treachery LocalTrouble Cards.localTrouble

-- | When Revealed: Attach this card to the hero with the highest threat cost without a
-- copy of Local Trouble attached. (Counts as a Condition attachment with the text: "When
-- attached hero exhausts, readies, or triggers an ability, raise its controller's threat
-- by 1.")
-- ponytail: read off the hero's exhausting, its readying in the refresh phase and its
-- abilities as they are used; a hero readied by a card effect outside the refresh phase
-- opens no window, so it raises nothing.
instance RunMessage LocalTrouble where
  runMessage msg t@(LocalTrouble attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      heroes <- traverse getHero =<< select AnyHero
      free <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode . toTarget) heroes
      let cost h = fromMaybe 0 $ cdThreatCost (toCardDef h)
          highest = foldr (max . cost) 0 free
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "localTrouble.attach"
        causedBy attrs
        targets [h | h <- free, cost h == highest] \h -> attachTreachery attrs.treacheryId h
      pure t
    CheckWindows ws | AttachedTo host <- attrs.placement -> do
      let n = length [() | Window After (Exhausted h) <- ws, h == host] + length [() | Window After (Readied h _) <- ws, h == host]
      trouble host n
      LocalTrouble <$> runMessage msg attrs
    UseAbility _ ab _ | AttachedTo host <- attrs.placement, sourceTarget ab.abilitySource == Just host -> do
      trouble host 1
      LocalTrouble <$> runMessage msg attrs
    _ -> LocalTrouble <$> runMessage msg attrs
   where
    trouble host n = when (n > 0) $ traverse_ (`encounterRaisesThreat` n) =<< characterController host
