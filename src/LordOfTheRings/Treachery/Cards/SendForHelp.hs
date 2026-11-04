module LordOfTheRings.Treachery.Cards.SendForHelp (
  sendForHelpLeadership,
  sendForHelpTactics,
  sendForHelpSpirit,
  sendForHelpLore,
) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (EncounterCard (..), cdName, display, genPlayerCard, toCardCode)
import LordOfTheRings.Card.Code (CardCode (..))
import LordOfTheRings.Card.CardDef (cdCardType)
import LordOfTheRings.Card.CardType (CardType (HeroType))
import LordOfTheRings.Hero.CardDefs (allHeroCards)
import LordOfTheRings.Helpers.Campaign (campaignUpgraded)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

-- | Send for Help, a boon of the Dream-chaser campaign printed with an encounter card's
-- text: set aside, Gavin shuffles one into the encounter deck.
newtype SendForHelp = SendForHelp TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sendForHelpLeadership :: TreacheryCard SendForHelp
sendForHelpLeadership = treachery SendForHelp Cards.sendForHelpLeadership

sendForHelpTactics :: TreacheryCard SendForHelp
sendForHelpTactics = treachery SendForHelp Cards.sendForHelpTactics

sendForHelpSpirit :: TreacheryCard SendForHelp
sendForHelpSpirit = treachery SendForHelp Cards.sendForHelpSpirit

sendForHelpLore :: TreacheryCard SendForHelp
sendForHelpLore = treachery SendForHelp Cards.sendForHelpLore

{- | Surge. Each: Remove Send for Help from the game. Shadow: Resolve this card's when
revealed effect.
Leadership: When Revealed: Choose a player to search the top 10 cards of their deck for a
card, draw it or play it at no cost, and shuffle their deck. If upgraded, repeat this
effect.
Tactics: When Revealed: Search the players' collections for a hero and put it into play
under any player's control. If upgraded, repeat this effect. At the end of the round, or if
those heroes leave play, remove them from the game.
Spirit: When Revealed: Choose one: resource, planning, combat, or refresh. After the
current phase, there is an additional instance of the chosen phase. If upgraded, choose
two instead.
Lore: When Revealed: Search the players' collections for Gandalf ally (Core Set 73) and
put him into play under any player's control, ignoring uniqueness. If upgraded, that
player chooses up to 3 of Gandalf's options instead of 1.

-}
instance RunMessage SendForHelp where
  runMessage msg t@(SendForHelp attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> resolve >> pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> resolve >> pure t
    Discard (isTarget attrs -> True) -> do
      t' <- SendForHelp <$> runMessage msg attrs
      removeFromEncounterDiscard (MkEncounterCard attrs.cardId (toCardCode attrs))
      pure t'
    _ -> SendForHelp <$> runMessage msg attrs
   where
    resolve = do
      lead <- getLead
      upgraded <- campaignUpgraded (toCardCode attrs)
      let times = if upgraded then 2 else 1
      case unCardCode (toCardCode attrs) of
        "DCC211" -> replicateM_ times $ choosePlayerM lead attrs \pid -> do
          deck <- (.deck) <$> getPlayer pid
          chooseOneM pid do
            questionLabeled "sendForHelp.search"
            causedBy attrs
            showing (take 10 deck)
            targets [CardIdTarget c.id | c <- take 10 deck] \case
              CardIdTarget cid -> for_ (find ((== cid) . (.id)) deck) (takeFromDeck pid)
              _ -> pure ()
            labeled "sendForHelp.nothing" $ pure ()
          shufflePlayerDeck pid
        "DCC212" -> replicateM_ times do
          inPlay <- map toCardCode <$> (traverse getHero =<< select AnyHero)
          let core = [def | def <- toList allHeroCards, cdCardType def == HeroType, unCardCode (toCardCode def) < "01013", toCardCode def `notElem` inPlay]
          choosePlayerM lead attrs \pid -> chooseOneM pid do
            questionLabeled "sendForHelp.hero"
            causedBy attrs
            for_ core \def -> labeledWith "sendForHelp.heroNamed" [textVar "hero" (display $ cdName def)] do
              card <- genPlayerCard def (Just pid)
              putHeroIntoPlay pid card
        "DCC213" -> replicateM_ times do
          chooseOneM lead do
            questionLabeled "sendForHelp.phase"
            causedBy attrs
            labeled "sendForHelp.resource" do
              eachPlayer \pid -> do
                heroes <- select (HeroControlledBy $ PlayerWithId pid)
                for_ heroes \h -> placeResources attrs h 1
                drawCards attrs pid 1
            labeled "sendForHelp.refresh" do
              characters <- selectCharacters ExhaustedHero ExhaustedAlly
              traverse_ encounterReadies characters
        _ -> choosePlayerM lead attrs \pid -> do
          card <- genPlayerCard Allies.gandalf (Just pid)
          putAllyIntoPlayUnder pid card
