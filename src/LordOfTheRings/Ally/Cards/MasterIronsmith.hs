module LordOfTheRings.Ally.Cards.MasterIronsmith (masterIronsmith) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), CardType (AttachmentType), toCardCode)
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Trait (Trait (Armor, Weapon))

newtype MasterIronsmith = MasterIronsmith AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

masterIronsmith :: AllyCard MasterIronsmith
masterIronsmith = ally MasterIronsmith Cards.masterIronsmith

-- | Response: After you play Master Ironsmith from your hand, if you paid all of its
-- resource cost from a single hero's resource pool, attach a Weapon or Armor attachment
-- from your hand or discard pile to that hero without paying its resource cost.
instance HasAbilities MasterIronsmith where
  getAbilities (MasterIronsmith a) = [mkAbility a 1 $ response $ PaidFromOneHeroWindow After (CardIdTarget a.allyCardId)]

instance RunMessage MasterIronsmith where
  runMessage msg s@(MasterIronsmith a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      for_ (take 1 [h | Window _ (PaidFrom _ (h : _)) <- windows]) \h -> do
        player <- getPlayer pid
        let gear = CardWithType AttachmentType <> CardMatchAny [CardWithTrait Weapon, CardWithTrait Armor]
            fits c = andM [passesUnique c, elem h <$> cardHosts pid (toCardCode c)]
        fromHand <- filterM fits [c | c <- player.hand, cardMatches gear c]
        fromDiscard <- filterM (fits . PlayerCard) [pc | PlayerCard pc <- player.discard, cardMatches gear pc]
        chooseOneM pid do
          questionLabeled "masterIronsmith.attach"
          causedBy a
          for_ fromHand \c -> targeting c $ attachFromHand pid c h
          for_ fromDiscard \pc -> targeting pc $ attachFromDiscard pid pc h
      pure s
    _ -> MasterIronsmith <$> runMessage msg a
