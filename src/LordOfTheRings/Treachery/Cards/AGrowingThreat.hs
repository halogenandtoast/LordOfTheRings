module LordOfTheRings.Treachery.Cards.AGrowingThreat (aGrowingThreat) where

import LordOfTheRings.Campaign (CampaignLog (campaignLogPool), CampaignLogUpdate (AddToPool), PoolCard (..))
import LordOfTheRings.Card (CardDef (..), CardType (ObjectiveAllyType, ObjectiveType), toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Campaign (getCampaignLog)
import LordOfTheRings.Trait (Trait (Burden))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AGrowingThreat = AGrowingThreat TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Ered Mithrin campaign, shuffled in by Journey Up the Anduin's campaign
-- card. Surge, on its card def.
aGrowingThreat :: TreacheryCard AGrowingThreat
aGrowingThreat = treachery AGrowingThreat Cards.aGrowingThreat

{- | When Revealed: Set aside the topmost non-unique, non-objective, non-burden encounter
card in the encounter discard pile. During quest resolution, add a copy of that card to
the campaign pool, if able. Remove A Growing Threat from the game.

The copy is written to the campaign log now: what the log gains during a scenario is kept
only once it is won, so it joins the pool as the scenario is resolved. "If able": no more
copies than one collection holds (MEC115 rules, "Campaign Pool"), its encounter set's.
-}
instance RunMessage AGrowingThreat where
  runMessage msg t@(AGrowingThreat attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discarded <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      let eligible c =
            let def = toCardDef c
             in not (cdUnique def) && cdCardType def `notElem` [ObjectiveType, ObjectiveAllyType] && Burden `notMember` cdCardTraits def
      for_ (find eligible discarded) \c -> do
        removeFromEncounterDiscard c
        setAside c
        getCampaignLog >>= traverse_ \l -> do
          let owned = fromMaybe 1 (cdEncounterSetQuantity $ toCardDef c)
              pooled = length [p | p <- campaignLogPool l, poolCardCode p == toCardCode c]
          when (pooled < owned) $ updateCampaignLog (AddToPool (PoolCard (toCardCode c) Nothing Nothing))
      removeFromGame attrs
      pure t
    _ -> AGrowingThreat <$> runMessage msg attrs
